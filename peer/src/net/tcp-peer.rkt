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
(define *USE-COMPRESSION?* #f)
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
  
  ;; input thread: look for either (1) an EOF, (2) a signal on the control channel to exit,
  ;; or (3) a message to read, deserialize and deliver across the designated reply-to thread.
  (define (run-input-thread i control-channel ra rp)
    (thread
     (λ ()
       (with-handlers ([exn? (make-abandon/signal i control-channel connects-i (cons ra rp))])
         (define sige (handle-evt control-channel done/signalled))
         (define reade (handle-evt i (curry read-in/forward-message reply-thread)))
         (let loop ()
           (sync sige reade)
           (loop))))))
  
  ;;; SENDING
  
  ;; called if output thread receives anything in mailbox.
  (define (output-next o)
    (define m (thread-receive))
    (cond [(list? m) (write-out o m)]
          [else (raise/ccm exn:fail "An invalid outgoing message was queued for writing")]))
  
  ;; output thread: look for either a message to send out or a signal to exit.
  (define (run-output-thread o control-channel ra rp)
    (thread
     (λ ()
       (with-handlers ([exn? (make-abandon/signal o control-channel connects-o (cons ra rp))])
         (define msge (handle-evt (thread-receive-evt) (λ _ (output-next o))))
         (define sige (handle-evt control-channel done/signalled))
         (let loop ()
           (sync msge sige)
           (loop))))))
  
  ;;; CONNECTING
  (define (do-accept)
    (let*-values ([(i o) (tcp-accept listener)])
      (define the-remote-scurl (handle-server-authentication this-scurl revoke? i o))
      (flush-output o)
      (cond [(scurl? the-remote-scurl)
             (define the-remote-url (scurl-url the-remote-scurl))
             (define ra (url-host the-remote-url))
             (define rp (url-port the-remote-url))
             (printf "accepted from ~a:~a~n" ra rp)
             (start-threads/store! i o ra rp)]
            [else (printf "not accepted: the returned scurl auth is ~a~n" the-remote-scurl)])
      (do-accept)))
  
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
    (let*-values ([(i o) (tcp-connect (request-host req) (request-port req))]
                  [(la lp ra rp) (tcp-addresses i #t)])
      ;; construct a SCURL expressing the intent of our connection request.
      ;; if the SCURL library returns a SCURL then we know the validation protocol succeeded.
      (define should-be-scurl (triple->scurl/defaults (request-host req) (request-port req) (request-key req)))
      (define the-remote-scurl (handle-client-authentication this-scurl should-be-scurl i o))
      (flush-output o)
      (cond [(scurl? the-remote-scurl)
             (printf "connected to ~a:~a~n" ra rp)
             (start-threads/store! i o ra rp)
             (hash-ref connects-o (cons ra rp))]
            [else (printf "not connected: the returned scurl auth is ~a~n" the-remote-scurl) #f])))
  
  ;; used by both the accepting and connecting processes to start and store threads
  ;; monitoring given output and input ports bound to the given canonical host:port
  (define/contract (start-threads/store! i o ra rp)
    (input-port? output-port? string? exact-nonnegative-integer? . -> . void)
    (define control-channel (make-async-channel))
    (define ot (run-output-thread o control-channel ra rp))
    (define it (run-input-thread i control-channel ra rp))
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

;; OUTPUT
(define (write-w/-compression msg o) (write (compress msg) o))
(define (write-w/o-compression msg o) (write msg o))
(define writerfunction (if *USE-COMPRESSION?* write-w/-compression write-w/o-compression))
(define (write-out o msg)
  (writerfunction msg o)
  (flush-output o))

;; INPUT
(define readerfunction (if *USE-COMPRESSION?* (compose decompress read) read))
(define (read-in/forward-message reply-thread i)
  (define m (readerfunction i))
  (thread-send reply-thread (deserialize m BASELINE #f)))