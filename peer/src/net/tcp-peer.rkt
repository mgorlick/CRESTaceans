#lang racket/base

(require racket/tcp
         racket/contract
         racket/function
         racket/async-channel
         "../api/compilation.rkt"  
         "structs.rkt"
         "compression.rkt"
         "encryption.rkt"
         "scurls.rkt")

(provide *LOCALHOST*
         run-tcp-peer
         generate-scurl/defaults
         generate-key/defaults
         (all-from-out "scurls.rkt"))

(define island-pair/c (cons/c string? exact-nonnegative-integer?))
(define msg? list?) ; the contract representing post-serialization messages

(define *LOCALHOST* "127.0.0.1")
(define *USE-COMPRESSION?* #t)
(print-graph #f)

;; run-tcp-peer: returns a thread handle used to communicate with the networking layer.
;; encapsulates worker threads for connection I/O, connection startup and keying and teardown, etc.
;; (thread-send (run-tcp-peer ...) (request ...)) results in a new message being sent.
(define/contract (run-tcp-peer hostname port this-scurl reply-thread)
  (string? exact-nonnegative-integer? private-scurl? thread? . -> . thread?)
  
  (define listener (tcp-listen port 64 #f hostname))
  
  (define/contract connects-o (hash/c island-pair/c thread?) (make-hash))
  (define/contract connects-i (hash/c island-pair/c thread?) (make-hash))
  
  (define (revoke? remote-scurl) #f)
  
  (define/contract (make-abandon/signal port control-channel hash self-key)
    (port? async-channel? (hash/c island-pair/c thread?) island-pair/c . -> . (exn? . -> . void))
    (λ (e)
      (printf "~a thread error: ~a~n" (if (input-port? port) "Input" "Output") e)
      (printf "terminating connection~n")
      (tcp-abandon-port port)
      (hash-remove! hash self-key)
      (async-channel-put control-channel 'exit)))
  
  ;; RECEIVING
  
  ;; called if input or output thread gets a signal through their distinguished
  ;; control async channel. typically one will signal the other.
  ;; (in the future, may need a third thread to share the ref)
  (define (done/signalled v)
    (when (equal? 'exit v)
      (raise/ccm exn:fail:network "Received close command")))
  
  (define/contract (input-next i decrypt)
    (input-port? (bytes? bytes? . -> . bytes?) . -> . void)
    (define encrypted-message (read i))
    (define message (read (open-input-bytes (decrypt (vector-ref encrypted-message 0) (vector-ref encrypted-message 1)))))
    (thread-send reply-thread (deserialize (decompress message) BASELINE #f)))
  
  ;; input thread: look for either (1) a signal on the control channel to exit,
  ;; or (3) a message to read, deserialize and deliver across the designated reply-to thread.
  (define/contract (run-input-thread i control-channel self-key decrypt)
    (input-port? async-channel? island-pair/c (bytes? bytes? . -> . bytes?) . -> . void)
    (thread
     (λ ()
       (with-handlers ([exn? (make-abandon/signal i control-channel connects-i self-key)])
         (define sige (handle-evt control-channel done/signalled))
         (define reade (handle-evt i (λ (i) (input-next i decrypt))))
         (let loop ()
           (sync sige reade)
           (loop))))))
  
  ;;; SENDING
  
  ;; called if output thread receives anything in mailbox.
  (define (output-next o encrypt)
    (define m (thread-receive))
    (cond [(list? m)
           (define compressed-message (compress m))
           (define b# (writable->bytes compressed-message))
           (define-values (cipher nonce) (encrypt b#))
           (write (vector cipher nonce) o)]
          [else (raise/ccm exn:fail "An invalid outgoing message was queued for writing")]))
  
  ;; output thread: look for either a message to send out or a signal to exit.
  (define/contract (run-output-thread o control-channel self-key encrypt)
    (output-port? async-channel? island-pair/c (bytes? . -> . (values bytes? bytes?)) . -> . void)
    (thread
     (λ ()
       (with-handlers ([exn? (make-abandon/signal o control-channel connects-o self-key)])
         (define msge (handle-evt (thread-receive-evt) (λ _ (output-next o encrypt))))
         (define sige (handle-evt control-channel done/signalled))
         (let loop ()
           (sync msge sige)
           (loop))))))
  
  ;;; CONNECTING
  (define (do-accept)
    (define-values (i o) (tcp-accept listener))
    (file-stream-buffer-mode o 'none)
    ;; first do the SCURL authentication protocol.
    (define-values (ra rp) (do-server-auth this-scurl revoke? i o))
    (cond [(and ra rp)
           (printf "accepted from ~a:~a~n" ra rp)
           ;; then do Diffie-Hellman key exchange.
           (define-values (my-PK set-PK) (make-pk/encrypter/decrypter))
           (define their-PK (do-DH-exchange my-PK i o))
           (define-values (encrypter decrypter) (set-PK their-PK))
           ;; finally, ready to run normal input and output threads, which handle their own encryption/decryption.
           (start-threads/store! i o ra rp encrypter decrypter)]
          [else #f])
    (do-accept))
  
  ;; Forward outbound message to the thread managing the appropriate output port.
  ;; if thread not found, connect and then try to send.
  ;; if no thread returned, assume connection attempt unauthorized.
  ;; FIXME: don't silently fail here. (requires widening the interface between this layer and above)
  (define (do-sending)
    (define/contract req request? (thread-receive))
    (define othread (hash-ref connects-o (cons (request-host req) (request-port req))
                              (λ () (connect/store! req))))
    (when othread (thread-send othread (request->serialized req) #f))
    (do-sending))
  
  ;; Do a synchronous tcp connect, then perform the client side of the SCURL authentication protocol.
  ;; finally, launch and register the input and output threads.
  (define/contract (connect/store! req)
    (request? . -> . (or/c #f thread?))  
    (define-values (i o) (tcp-connect (request-host req) (request-port req)))
    (file-stream-buffer-mode o 'none)
    (define-values (la lp ra rp) (tcp-addresses i #t))
    ;; first do the SCURL authentication protocol.
    (define the-remote-scurl (do-client-auth (request-host req) (request-port req) (request-key req) this-scurl i o))
    (cond [the-remote-scurl
           (printf "connected to ~a:~a~n" ra rp)
           ;; then do Diffie-Hellman key exchange.
           (define-values (my-PK set-PK) (make-pk/encrypter/decrypter))
           (define their-PK (do-DH-exchange my-PK i o))
           (define-values (encrypter decrypter) (set-PK their-PK))
           ;; finally, ready to run normal input and output threads, which handle their own encryption/decryption.
           (start-threads/store! i o ra rp encrypter decrypter)
           (hash-ref connects-o (cons ra rp))]
          [else (printf "not connected: the returned scurl auth is ~a~n" the-remote-scurl) #f]))
  
  ;; used by both the accepting and connecting processes to start and store threads
  ;; monitoring given output and input ports bound to the given canonical host:port
  (define/contract (start-threads/store! i o ra rp encrypter decrypter)
    (input-port? output-port? string? exact-nonnegative-integer? encrypter/c decrypter/c . -> . void)
    (define control-channel (make-async-channel))
    (define ot (run-output-thread o control-channel (cons ra rp) encrypter))
    (define it (run-input-thread i control-channel (cons ra rp) decrypter))
    (hash-set! connects-o (cons ra rp) ot)
    (hash-set! connects-i (cons ra rp) it))
  
  ;; -------------------------------------
  
  ;; one thread to manage all tcp-accepts.  
  (define accepter (thread do-accept))
  ;; one thread to monitor the outgoing messages and redirect them
  (define sendmaster (thread do-sending))
  sendmaster)

;; raise an exception given the exception's type and message.
;; this just alleviates having to type (current-continuation-marks) a lot
(define/contract (raise/ccm f msg)
  ((string? continuation-mark-set? . -> . exn?) string? . -> . exn?)
  (raise (f msg (current-continuation-marks))))

(define/contract (request->serialized req)
  (request? . -> . msg?)
  (serialize (request-message req)))

(define (writable->bytes t)
  (define o (open-output-bytes))
  (write t o)
  (get-output-bytes o))