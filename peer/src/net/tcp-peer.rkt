#lang racket/base

(require racket/tcp
         racket/contract
         racket/async-channel
         racket/stream
         racket/unit
         ffi/unsafe/atomic
         "../../../Motile/compile/serialize.rkt"
         "../../../Motile/actor/curl.rkt"
         "../../../Motile/actor/island.rkt"
         "compression.rkt"
         "encryption.rkt")

(provide (except-out (all-defined-out)
                     output-next
                     input-next
                     done/signalled
                     make-abandon/signal
                     run-input-loop
                     run-output-loop
                     writable->bytes
                     mbox->stream))

(struct exn:fail:network:scurl exn:fail:network ())

(define msg/c list?) ; the contract representing post-serialization messages
(define hostname/c string?)
(define portname/c exact-nonnegative-integer?)
(define island-pair/c (cons/c hostname/c portname/c))

(define connect-responder/c
  (input-port? output-port? hostname/c portname/c encrypter/c decrypter/c async-channel? . -> . any/c))
(define output-msg-retriever/c (-> any/c))
(define input-msg-handler/c (any/c . -> . any/c))

(define *LOCALHOST* "127.0.0.1")
(print-graph #f)

(define num-connect-tries (make-parameter 8))
(define secs-between-connect-attempts (make-parameter 1))
(define (revoke? remote-scurl) #f)

; note on the use of async channels in this module:
; for each pair of input and output threads spawned (i.e., those two threads monitoring
; the ports of a tcp connection), an async channel is created and shared.
; that channel is used to send termination signals back and forth: if one i/o thread raises
; puts an exit signal onto the channel, the other will catch it and also exit.
; this requires the orchestration logic to make an exception handler via
; `make-abandon/signal' and install it as the exception handler over one i/o thread.
; this should happen in the body of the user-provided `connect-responder/c'.

;; run-tcp-peer: returns a thread handle used to communicate with the networking layer.
;; encapsulates worker threads for connection I/O, connection startup and keying and teardown, etc.
;; (thread-send (run-tcp-peer ...) (request ...)) results in a new message being sent.
(define/contract (run-tcp-peer lhostname lport reply-thread #:encrypt? [encrypt? #t])
  ([hostname/c portname/c thread?] [#:encrypt? boolean?] . ->* . thread?)
  
  (define listener (tcp-listen lport 64 #f lhostname))
  
  ;; hold output ports for active connections.
  (define/contract connects-o (hash/c island-pair/c thread?) (make-hash))
  ;; hold input ports for active connections.
  (define/contract connects-i (hash/c island-pair/c thread?) (make-hash))
  
  ;;; CONNECTING
  
  (define/contract (launch-input-thread i control-channel decrypter remote-id)
    (input-port? async-channel? decrypter/c island-pair/c . -> . thread?)
    (thread
     (λ ()
       (define exiter (make-abandon/signal i control-channel))
       (with-handlers ([exn? (λ (e)
                               (exiter e)
                               (hash-remove! connects-i remote-id))])
         (run-input-loop i control-channel decrypter 
                         (λ (m) (thread-send reply-thread m #f)))))))
  
  ;; used by the accepting process to start and store threads
  ;; monitoring given output and input ports bound to the given canonical host:port
  (define/contract (start-threads/store! i o ra rp encrypter decrypter control-channel)
    connect-responder/c
    (define remote-id (cons ra rp))
    (define ot (thread
                (λ ()
                  (define exiter (make-abandon/signal o control-channel))
                  (with-handlers ([exn? (λ (e)
                                          (exiter e)
                                          (hash-remove! connects-o remote-id))])
                    (run-output-loop o control-channel encrypter thread-receive)))))
    (define it (launch-input-thread i control-channel decrypter remote-id))
    (hash-set! connects-o remote-id ot)
    (hash-set! connects-i remote-id it))
  
  ;; -------------------------------------
  
  (define/contract (send/maybe-connect req)
    ((cons/c curl? any/c) . -> . void)
    (define the-address (curl/island (car req)))
    (define remote-id (cons (bytes->string/utf-8 (island/address/dns the-address))
                            (island/address/port the-address)))
    (with-handlers ([exn? (λ (e) (printf "Send/maybe-connect: ~a~n" e))])
      (cond [(hash-has-key? connects-o remote-id)
             ;; there's a connection active - forward the message to it and return.
             (thread-send (hash-ref connects-o remote-id) (->serialized req) #f)]
            [else
             ;; launch a new thread that will LATER turn into the output thread,
             ;; but first acts as the connecting thread.
             ;; turn off threading here so that there's always an entry in the
             ;; `connects-o' hash table for `try-connect-n-times' to remove, when necessary.
             (start-atomic)
             (define connector-t (run-connector remote-id req))
             (thread-send connector-t (->serialized req))
             (hash-set! connects-o remote-id connector-t)
             (end-atomic)])))
  
  ;; used for the connecting process (i.e. called only by the thread spawned
  ;; inside `send/maybe-connect') to store itself as the output thread, and
  ;; a new thread as the input thread for a connection
  (define (run-connector remote-id req)
    
    (define the-address (curl/island (car req)))
    (define dns (bytes->string/utf-8 (island/address/dns the-address)))
    (define port (island/address/port the-address))
    
    (define/contract (setup-connection! i o ra rp encrypter decrypter control-channel)
      connect-responder/c
      (define remote-id (cons ra rp))
      (define it (launch-input-thread i control-channel decrypter remote-id))
      (hash-set! connects-i remote-id it)
      ;; the output thread is already set (in send/maybe-connect).
      (define exiter (make-abandon/signal o control-channel))
      ;; now turn into the output thread.
      (with-handlers ([exn? (λ (e) (exiter e) (hash-remove! connects-o remote-id))])
        (run-output-loop o control-channel encrypter thread-receive)))
    
    (define (try-connect-n-times n sleeptime)
      (with-handlers ([exn:fail:network:scurl? 
                       ; the connection may have succeeded, but the SCURL 
                       ; auth protocol failed. just try-connect-n-times up and bail.
                       (λ (e) (printf "~a~n" e) (try-connect-n-times n sleeptime))]
                      ; connection couldn't be made.
                      ; try again after waiting the policy-designated sleep time
                      [exn:fail:network? (λ (e)
                                           (sleep sleeptime)
                                           (try-connect-n-times (sub1 n) (* sleeptime 2)))])
        (cond [(zero? n) 
               (printf "Connect attempts exceeded. Dropping all messages to ~a~n" remote-id)
               (hash-remove! connects-o remote-id)]
              [else
               (displayln "Trying a connect")
               (connect lport dns port setup-connection!)])))
    (thread (λ () (try-connect-n-times (num-connect-tries) (secs-between-connect-attempts)))))
  
  ;; -------------------------------------
  
  ;; encryption stuff.
  (define/contract (do-key-exchange/make-encrypter/decrypter remote-encrypt-wanted? i o)
    (boolean? input-port? output-port? . -> . (values encrypter/c decrypter/c))
    ;; choice of encryption.
    (define-values/invoke-unit (if (and encrypt? remote-encrypt-wanted?) nacl-encryption@ no-encryption@)
      (import)
      (export encryption-unit^))
    ;; then do Diffie-Hellman key exchange.
    (define-values (my-PK set-PK) (make-pk/encrypter/decrypter))
    (define their-PK (do-DH-exchange my-PK i o))
    (set-PK their-PK))
  
  ;; -------------------------------------
  
  ;; Do a synchronous tcp connect, then perform the client side of the SCURL authentication protocol.
  ;; finally, launch and register the input and output threads.
  (define/contract (connect lport host port finish-connect)
    (portname/c hostname/c portname/c connect-responder/c . -> . any/c)
    (define-values (i o) (tcp-connect host port))
    (file-stream-buffer-mode o 'none)
    (define-values (la _ ra rp) (tcp-addresses i #t))
    
    (write (vector la lport encrypt?) o)
    (define-values (ra* rp* remote-encrypt?) (vector->values (read i)))
    (cond [#t ; placeholder for scurl client side auth protocol check.
           (printf "~a:~a: connected to ~a:~a~n" la lport ra rp)
           (define-values (encrypter decrypter) (do-key-exchange/make-encrypter/decrypter remote-encrypt? i o))
           ;; finally, ready to run normal input and output threads,
           ;; which handle their own encryption/decryption.
           (finish-connect i o ra* rp* encrypter decrypter (make-async-channel))]
          [else (tcp-abandon-port o)
                (tcp-abandon-port i)
                (raise/ccm exn:fail:network:scurl 
                           "not connected: failed the SCURL client-side auth protocol")]))
  
  ; run-accept-loop: sit on a tcp listener forever, accepting new connections.
  (define/contract (run-accept-loop listener finish-connect)
    (tcp-listener? connect-responder/c . -> . any/c)
    (with-handlers ([exn? (λ (e) (printf "~a~n" (exn-message e)) #f)])
      (define-values (i o) (tcp-accept listener))
      (file-stream-buffer-mode o 'none)
      ;; first do the SCURL authentication protocol.
      (write (vector lhostname lport encrypt?) o)
      (define-values (ra rp remote-encrypt?) (vector->values (read i)))
      (cond [(and ra rp)
             (printf "accepted from ~a:~a~n" ra rp)
             (define-values (encrypter decrypter) (do-key-exchange/make-encrypter/decrypter remote-encrypt? i o))
             ;; finally, ready to run normal input and output threads, which handle their own encryption/decryption.
             (finish-connect i o ra rp encrypter decrypter (make-async-channel))]
            [else (tcp-abandon-port o)
                  (tcp-abandon-port i)
                  (raise/ccm exn:fail:network:scurl
                             "not connected: failed the SCURL server-side auth protocol")]))
    (run-accept-loop listener finish-connect))
  
  ;; one thread to manage all tcp-accepts.  
  (define accepter (thread (λ () (run-accept-loop listener start-threads/store!))))
  ;; one thread to monitor the outgoing messages and redirect them
  (define sendmaster (thread (λ () (stream-for-each send/maybe-connect (mbox->stream)))))
  
  sendmaster)

;; -------------------------------------

;; called if input or output thread gets a signal through their distinguished
;; control async channel. typically one will signal the other.
;; (in the future, may need a third thread to share the ref. in that case,
; that third thread should be responsible for signalling both.)
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

;; input thread: look for either (1) a signal on the control channel to exit,
;; or (2) a message to read, deserialize and deliver via the provided handler.
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

;; output thread: look for either a message to send out or a signal to exit.
(define/contract (run-output-loop o control-channel encrypt getmsg)
  (output-port? async-channel? encrypter/c output-msg-retriever/c . -> . void)
  (define msge (handle-evt (thread-receive-evt) (λ _ (output-next o (getmsg) encrypt))))
  (define sige (handle-evt control-channel done/signalled))
  (let loop ()
    (sync msge sige)
    (loop)))

(define (->serialized req)
  (motile/serialize req))

;; given an outgoing message m, encrypt and compress m and then write it to the output port o.
(define/contract (output-next o m encrypt)
  (output-port? msg/c encrypter/c . -> . void)
  (define the-message-compressed (compress m))
  (define-values (cipher nonce) (encrypt (writable->bytes (compress m))))
  (write (vector cipher nonce) o))


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

(define (mbox->stream [end? (λ _ #f)])
  (define v (thread-receive))
  (if (end? v)
      empty-stream
      (stream-cons v (mbox->stream end?))))