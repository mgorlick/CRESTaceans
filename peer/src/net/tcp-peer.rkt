#lang racket/base

(require racket/tcp
         racket/contract
         racket/async-channel
         racket/provide
         racket/stream
         ffi/unsafe/atomic
         "../../../Motile/compile/serialize.rkt"
         "compression.rkt"
         "encryption.rkt"
         "scurls.rkt")

(provide generate-scurl/defaults
         generate-key/defaults
         (all-from-out "scurls.rkt")
         (except-out (all-defined-out)
                     output-next
                     input-next
                     done/signalled
                     make-abandon/signal
                     run-input-loop
                     run-output-loop
                     run-accept-loop
                     connect
                     writable->bytes
                     mbox->stream))

(struct request (host port key message))

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
(define/contract (run-tcp-peer lhostname lport this-scurl reply-thread)
  (hostname/c portname/c private-scurl? thread? . -> . thread?)
  
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
    (hash-set! connects-i remote-id it)
    (displayln connects-i)
    (displayln connects-o))
  
  
  ;; -------------------------------------
  
  ;; used for the connecting process (i.e. called only by the thread spawned
  ;; inside `send/maybe-connect') to store itself as the output thread, and
  ;; a new thread as the input thread for a connection
  (define (run-connector remote-id req)
    
    (define/contract (setup-connection! i o ra rp encrypter decrypter control-channel)
      connect-responder/c
      (define remote-id (cons ra rp))
      (define it (launch-input-thread i control-channel decrypter remote-id))
      (hash-set! connects-i remote-id it)
      
      (displayln connects-i)
      (displayln connects-o)
      
      ;; the output thread is already set (in send/maybe-connect).
      (define exiter (make-abandon/signal o control-channel))
      ;; now turn into the output thread.
      (with-handlers ([exn? (λ (e) (exiter e) (hash-remove! connects-o remote-id))])
        (run-output-loop o control-channel encrypter thread-receive)))
    
    (thread 
     (λ ()
       (let connectloop ([c (num-connect-tries)] [sleeptime (secs-between-connect-attempts)])
         (with-handlers ([exn:fail:network:scurl? 
                          ; the connection may have succeeded, but the SCURL 
                          ; auth protocol failed.just clean up and bail.
                          (λ (e) (printf "~a~n" e) (connectloop 0 sleeptime))]
                         ; connection couldn't be made.
                         ; try again after waiting the policy-designated sleep time
                         [exn:fail:network? (λ (e)
                                              (sleep sleeptime)
                                              (connectloop (sub1 c) (* sleeptime 2)))])
           (cond [(zero? c) 
                  (printf "Connect attempts exceeded. Dropping all messages to ~a~n" remote-id)
                  (hash-remove! connects-o remote-id)]
                 [else
                  (connect lport (request-host req) (request-port req) setup-connection!)]))))))
  
  (define/contract (send/maybe-connect req)
    (request? . -> . void)
    (define (->ser req) (motile/serialize (request-message req)))
    (define remote-id (cons (request-host req) (request-port req)))
    
    (cond [(hash-has-key? connects-o remote-id)
           ;; there's a connection active - forward the message to it and return.
           (thread-send (hash-ref connects-o remote-id) (->ser req) #f)]
          
          [else
           ;; launch a new thread that will LATER turn into the output thread,
           ;; but first acts as the connecting thread.
           ;; turn off threading here so that there's always an entry in the
           ;; `connects-o' hash table for `connectloop' to remove, when necessary.
           (start-atomic)
           (define connector-t (run-connector remote-id req))
           (thread-send connector-t (->ser req))
           (hash-set! connects-o remote-id connector-t)
           (end-atomic)]))
  
  ;; -------------------------------------
  
  ;; one thread to manage all tcp-accepts.  
  (define accepter (thread (λ () (run-accept-loop listener start-threads/store! this-scurl))))
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

;; given an outgoing message m, encrypt and compress m and then write it to the output port o.
(define/contract (output-next o m encrypt)
  (output-port? msg/c encrypter/c . -> . void)
  (define-values (cipher nonce) (encrypt (writable->bytes (compress m))))
  (write (vector cipher nonce) o))

;; -------------------------------------

(struct exn:fail:network:scurl exn:fail:network ())

; run-accept-loop: sit on a tcp listener forever, accepting new connections.
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
                (raise/ccm exn:fail:network:scurl
                           "not connected: failed the SCURL server-side auth protocol")]))
  (run-accept-loop listener f this-scurl))

;; Do a synchronous tcp connect, then perform the client side of the SCURL authentication protocol.
;; finally, launch and register the input and output threads.
(define/contract (connect lport host port f)
  (portname/c hostname/c portname/c connect-responder/c . -> . any/c)
  (define-values (i o) (tcp-connect host port))
  (file-stream-buffer-mode o 'none)
  (define-values (la _ ra rp) (tcp-addresses i #t))
  
  ;; first do the SCURL authentication protocol.
  ;(define the-remote-scurl (do-client-auth (request-host req) (request-port req) (request-key req) this-scurl i o))
  ;(cond [(scurl? the-remote-scurl)
  
  (write (vector la lport) o)
  (cond [#t
         
         (printf "~a:~a: connected to ~a:~a~n" la lport ra rp)
         
         ;; then do Diffie-Hellman key exchange.
         (define-values (my-PK set-PK) (make-pk/encrypter/decrypter))
         (define their-PK (do-DH-exchange my-PK i o))
         (define-values (encrypter decrypter) (set-PK their-PK))
         
         ;; finally, ready to run normal input and output threads,
         ;; which handle their own encryption/decryption.
         (f i o ra rp encrypter decrypter (make-async-channel))]
        [else (tcp-abandon-port o)
              (tcp-abandon-port i)
              (raise/ccm exn:fail:network:scurl 
                         "not connected: failed the SCURL client-side auth protocol")]))

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

(require "../../../Motile/tests/compile-test.rkt"
         "../../../Motile/compile/compile.rkt"
         "../../../Motile/baseline.rkt"
         "../../../Motile/generate/baseline.rkt")
(define (Motile-tests)
  (define-values (pk-A crypt-factory-A) (make-pk/encrypter/decrypter))
  (define-values (pk-B crypt-factory-B) (make-pk/encrypter/decrypter))
  (define-values (encrypt-A decrypt-A) (crypt-factory-A pk-B))
  (define-values (encrypt-B decrypt-B) (crypt-factory-B pk-A))
  
  (define compile/serialize/start
  (lambda (e) (motile/call (motile/deserialize (motile/serialize (motile/compile e)) #f)
                           ENVIRON/TEST)))
  
  (define (compile/encrypt/serialize/start e)
    (define m (motile/serialize (motile/compile e)))
    (define-values (cipher nonce) (encrypt-A (writable->bytes m)))
    (define decrypted-expr (decrypt-B cipher nonce))
    (motile/call (motile/deserialize (read (open-input-bytes decrypted-expr)) #f) 
                 ENVIRON/TEST))
  
  (define (compile/compress/encrypt/serialize/start e)
    (define m (motile/serialize (motile/compile e)))
    (define-values (cipher nonce) (encrypt-A (writable->bytes (compress m))))
    (define decrypted-expr (decrypt-B cipher nonce))
    (motile/call (motile/deserialize (decompress (read (open-input-bytes decrypted-expr))) #f)
                 ENVIRON/TEST))
  
  (parameterize ([compile/start compile/serialize/start])
    (compile/tests/all))
  (parameterize ([compile/start compile/encrypt/serialize/start])
    (compile/tests/all))
  (parameterize ([compile/start compile/compress/encrypt/serialize/start])
    (compile/tests/all)))