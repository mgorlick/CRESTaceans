#lang racket/base

(require racket/tcp
         racket/contract
         racket/async-channel
         racket/provide
         "../../../Motile/compile/serialize.rkt"
         "compression.rkt"
         "encryption.rkt"
         "scurls.rkt")

(provide *LOCALHOST*
         run-tcp-peer
         generate-scurl/defaults
         generate-key/defaults
         (matching-identifiers-out #rx"request.*" (all-defined-out))
         (all-from-out "scurls.rkt"))

(struct request (host port key message))

(define island-pair/c (cons/c string? exact-nonnegative-integer?))
(define msg/c list?) ; the contract representing post-serialization messages
(define hostname/c string?)
(define portname/c exact-nonnegative-integer?)
(define connect-responder/c
  (input-port? output-port? hostname/c portname/c encrypter/c decrypter/c async-channel? . -> . any/c))

(define *LOCALHOST* "127.0.0.1")
(print-graph #f)

(define (revoke? remote-scurl) #f)

;; run-tcp-peer: returns a thread handle used to communicate with the networking layer.
;; encapsulates worker threads for connection I/O, connection startup and keying and teardown, etc.
;; (thread-send (run-tcp-peer ...) (request ...)) results in a new message being sent.
(define/contract (run-tcp-peer hostname port this-scurl reply-thread)
  (hostname/c portname/c private-scurl? thread? . -> . thread?)
  
  (define listener (tcp-listen port 64 #f hostname))
  
  (define/contract connects-o (hash/c island-pair/c thread?) (make-hash))
  (define/contract connects-i (hash/c island-pair/c thread?) (make-hash))
  
  ;;; CONNECTING
  
  ;; used by both the accepting and connecting processes to start and store threads
  ;; monitoring given output and input ports bound to the given canonical host:port
  (define/contract (start-threads/store! i o ra rp encrypter decrypter control-channel)
    connect-responder/c
    (define self-key (cons ra rp))
    (define ot (thread
                (λ ()
                  (define exiter (make-abandon/signal o control-channel))
                  (with-handlers ([exn? (λ (e)
                                          (exiter e)
                                          (hash-remove! connects-o self-key))])
                    (run-output-loop o control-channel encrypter thread-receive)))))
    (define it (thread
                (λ ()
                  (define exiter (make-abandon/signal i control-channel))
                  (with-handlers ([exn? (λ (e)
                                          (exiter e)
                                          (hash-remove! connects-i self-key))])
                    (run-input-loop i control-channel decrypter 
                                    (λ (m) (thread-send reply-thread m #f)))))))
    (hash-set! connects-o self-key ot)
    (hash-set! connects-i self-key it))
  
  ;; -------------------------------------
  
  (define (get-output-thread/maybe-connect req)
    (hash-ref connects-o (cons (request-host req) (request-port req))
              (λ ()
                (with-handlers ([exn:fail:network? (λ (e) (printf "~a~n" (exn-message e)) #f)])
                  (connect (request-host req) (request-port req) start-threads/store!)
                  (hash-ref connects-o (cons (request-host req) (request-port req)))))))
  
  ;; Forward outbound message to the thread managing the appropriate output port.
  ;; if no thread returned, assume connection attempt unauthorized.
  (define (run-sending-loop)
    (define/contract req request? (thread-receive))
    (define othread (get-output-thread/maybe-connect req))
    (if othread
        (thread-send othread (motile/serialize (request-message req)) #f)
        (printf "unable to connect to host ~a:~a~n" (request-host req) (request-port req)))
    (run-sending-loop))
  
  ;; -------------------------------------
  
  ;; one thread to manage all tcp-accepts.  
  (define accepter (thread (λ () (run-accept-loop listener start-threads/store! this-scurl))))
  ;; one thread to monitor the outgoing messages and redirect them
  (define sendmaster (thread run-sending-loop))
  
  sendmaster)

;; -------------------------------------

;; called if input or output thread gets a signal through their distinguished
;; control async channel. typically one will signal the other.
;; (in the future, may need a third thread to share the ref)
(define (done/signalled v)
  (when (equal? 'exit v)
    (raise/ccm exn:fail:network "Received close command")))

;; produce a function that should be fired when a connection needs to be 
;; terminated, given the exn that caused the termination.
(define/contract (make-abandon/signal port control-channel)
  (port? async-channel? . -> . (exn? . -> . void))
  (λ (e)
    (printf "~a thread error: ~a~n" (if (input-port? port) "Input" "Output") e)
    (printf "terminating connection~n")
    (tcp-abandon-port port)
    (async-channel-put control-channel 'exit)))

;; -------------------------------------

;; RECEIVING

(define input-msg-handler/c (any/c . -> . any/c))

;; input thread: look for either (1) a signal on the control channel to exit,
;; or (2) a message to read, deserialize and deliver across the designated reply-to thread.
(define/contract (run-input-loop i control-channel decrypt msghandler)
  (input-port? async-channel? decrypter/c input-msg-handler/c . -> . void)
  (define sige (handle-evt control-channel done/signalled))
  (define reade (handle-evt i (λ _ (input-next i decrypt msghandler))))
  (let loop ()
    (sync sige reade)
    (loop)))

;; read a message from i. decrypt, decompress and deserialize it. then call the 
;; provided function to do something with the message.
(define/contract (input-next i decrypt f)
  (input-port? decrypter/c input-msg-handler/c . -> . any/c)
  (define encrypted-message (read i))
  (define message 
    (read (open-input-bytes (decrypt (vector-ref encrypted-message 0)
                                     (vector-ref encrypted-message 1)))))
  (f (motile/deserialize (decompress message) #f)))

;; -------------------------------------

;; SENDING

(define output-msg-retriever/c (-> any/c))

;; output thread: look for either a message to send out or a signal to exit.
(define/contract (run-output-loop o control-channel encrypt getmsg)
  (output-port? async-channel? encrypter/c output-msg-retriever/c . -> . void)
  (define msge (handle-evt (thread-receive-evt) (λ _ (output-next o (getmsg) encrypt))))
  (define sige (handle-evt control-channel done/signalled))
  (let loop ()
    (sync msge sige)
    (loop)))

;; given an outgoing message m, encrypt and compress m and then write it to the output port o.
(define/contract (output-next o m encrypt)
  (output-port? msg/c encrypter/c . -> . void)
  (define-values (cipher nonce) (encrypt (writable->bytes (compress m))))
  (write (vector cipher nonce) o))

;; -------------------------------------

(define/contract (run-accept-loop listener f this-scurl)
  (tcp-listener? connect-responder/c private-scurl? . -> . any/c)
  (with-handlers ([exn? (λ (e) (printf "~a~n" (exn-message e)) #f)])
    (define-values (i o) (tcp-accept listener))
    (file-stream-buffer-mode o 'none)
    ;; first do the SCURL authentication protocol.
    ;(define-values (ra rp) (do-server-auth this-scurl revoke? i o))
    (define-values (ra rp) (vector->values (read i)))
    (cond [(and ra rp)
           (printf "accepted from ~a:~a~n" ra rp)
           ;; then do Diffie-Hellman key exchange.
           (define-values (my-PK set-PK) (make-pk/encrypter/decrypter))
           (define their-PK (do-DH-exchange my-PK i o))
           (define-values (encrypter decrypter) (set-PK their-PK))
           ;; finally, ready to run normal input and output threads, which handle their own encryption/decryption.
           (f i o ra rp encrypter decrypter (make-async-channel))]
          [else (tcp-abandon-port o)
                (tcp-abandon-port i)
                (raise/ccm exn:fail:network
                           "not connected: failed the SCURL server-side auth protocol")]))
  (run-accept-loop listener f this-scurl))

;; Do a synchronous tcp connect, then perform the client side of the SCURL authentication protocol.
;; finally, launch and register the input and output threads.
(define/contract (connect host port f)
  (hostname/c portname/c connect-responder/c . -> . any/c)
  (define-values (i o) (tcp-connect host port))
  (file-stream-buffer-mode o 'none)
  (define-values (la lp ra rp) (tcp-addresses i #t))
  
  ;; first do the SCURL authentication protocol.
  ;(define the-remote-scurl (do-client-auth (request-host req) (request-port req) (request-key req) this-scurl i o))
  ;(cond [(scurl? the-remote-scurl)
  
  (write (vector la port) o)
  (cond [#t
         
         (printf "connected to ~a:~a~n" ra rp)
         
         ;; then do Diffie-Hellman key exchange.
         (define-values (my-PK set-PK) (make-pk/encrypter/decrypter))
         (define their-PK (do-DH-exchange my-PK i o))
         (define-values (encrypter decrypter) (set-PK their-PK))
         
         ;; finally, ready to run normal input and output threads,
         ;; which handle their own encryption/decryption.
         (f i o ra rp encrypter decrypter (make-async-channel))]
        [else (tcp-abandon-port o)
              (tcp-abandon-port i)
              (raise/ccm exn:fail:network "not connected: failed the SCURL client-side auth protocol")]))

;; -------------------------------------

;; raise an exception given the exception's type and message.
;; this just alleviates having to type (current-continuation-marks) a lot
(define/contract (raise/ccm f msg)
  ((string? continuation-mark-set? . -> . exn?) string? . -> . exn?)
  (raise (f msg (current-continuation-marks))))

(define (writable->bytes t)
  (define o (open-output-bytes))
  (write t o)
  (get-output-bytes o))