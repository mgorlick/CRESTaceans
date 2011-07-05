#lang racket/base

(require racket/tcp
         racket/contract
         racket/port
         racket/function
         racket/async-channel
         (planet "main.rkt" ("soegaard" "gzip.plt" 2 2))
         "../api/compilation.rkt"  
         "structs.rkt"
         "scurl/peer-validation/depends.rkt"
         "scurl/peer-validation/scurl.rkt"
         "scurl/peer-validation/peer-validation.rkt")

(provide *LOCALHOST*
         run-tcp-peer
         generate-scurl/defaults
         generate-key/defaults
         (all-from-out "scurl/peer-validation/scurl.rkt"))

(define island-pair/c (cons/c string? exact-nonnegative-integer?))
(define portHT/c (hash/c island-pair/c thread?))

(define *LOCALHOST* "127.0.0.1")
(define *USE-COMPRESSION?* #f)
(print-graph #f)

;; run-tcp-peer: returns a thread handle used to communicate with the networking layer.
;; encapsulates worker threads for connection I/O, connection startup and keying and teardown, etc.
;; (thread-send (run-tcp-peer ...) (request ...)) results in a new message being sent.
(define/contract (run-tcp-peer hostname port this-scurl reply-thread)
  (string? exact-nonnegative-integer? private-scurl? thread? . -> . thread?)
  
  (define listener (tcp-listen port 64 #f hostname))
  
  (define connects-o (make-hash)) ; (hash/c island-pair/c thread?)
  (define connects-i (make-hash)) ; (hash/c island-pair/c thread?)
  
  (define (revoke? remote-scurl)
    #f)
  
  (define (make-abandon/signal port control-channel self-key)
    (λ (e)
      (printf "~a thread error: ~a~n" (if (input-port? port) "Input" "Output") e)
      (printf "terminating connection~n")
      (tcp-abandon-port port)
      (if (input-port? port)
          (hash-remove! connects-i self-key)
          (hash-remove! connects-o self-key))
      (async-channel-put control-channel 'exit)))
  
  ;; RECEIVING
  
  ;; called if input or output thread gets a signal through their distinguished
  ;; control async channel. typically one will signal the other.
  ;; (in the future, may need a third thread to share the ref)
  (define (done/signalled v)
    (when (equal? 'exit v)
      (raise (make-exn:fail:network "Received close command" (current-continuation-marks)))))
  
  ;; input thread: look for either (1) an EOF, (2) a signal on the control channel to exit,
  ;; or (3) a message to read, deserialize and deliver across the designated reply-to thread.
  (define (run-input-thread i control-channel ra rp)
    (thread
     (λ ()
       (with-handlers ([exn? (make-abandon/signal i control-channel (cons ra rp))])
         (define sige (handle-evt control-channel done/signalled))
         (define reade (handle-evt i (curry read-in/forward-message reply-thread)))
         (let loop ()
           (sync sige reade)
           (loop))))))
  
  ;;; SENDING
  
  ;; called if output thread receives anything in mailbox.
  (define (output-next o)
    (define m (thread-receive))
    (cond [(list? m) (write-out o m)])) ; messages are already lists in serialized form
  
  ;; output thread: look for either a message to send out or a signal to exit.
  (define (run-output-thread o control-channel ra rp)
    (thread
     (λ ()
       (with-handlers ([exn? (make-abandon/signal o control-channel (cons ra rp))])
         (define msge (handle-evt (thread-receive-evt) (λ _ (output-next o))))
         (define sige (handle-evt control-channel done/signalled))
         (let loop ()
           (sync msge sige)
           (loop))))))
  
  ;;; CONNECTING
  (define (do-accepts)
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
      (do-accepts)))
  
  ;; Forward outbound message to the thread managing the appropriate output port.
  ;; if thread not found, connect and then try to send.
  ;; if no thread returned, assume connection attempt unauthorized.
  ;; FIXME: don't silently fail here. (requires widening the interface between this layer and above)
  (define (do-sending)
    (define req (thread-receive))
    (cond [(request? req)
           (define othread (hash-ref connects-o (cons (request-host req) (request-port req))
                                     (λ () (connect/store! req))))
           (when othread
             (thread-send othread (request->serialized req) #f))
           (do-sending)]))
  
  (define (connect/store! req)
    (let*-values ([(i o) (tcp-connect (request-host req) (request-port req))]
                  [(la lp ra rp) (tcp-addresses i #t)])
      (define the-remote-scurl
        (handle-client-authentication
         this-scurl (triple->scurl/defaults (request-host req) (request-port req) (request-key req)) i o))
      (flush-output o)
      (cond [(scurl? the-remote-scurl)
             (printf "connected to ~a:~a~n" ra rp)
             (start-threads/store! i o ra rp)
             (hash-ref connects-o (cons ra rp))]
            [else (printf "not connected: the returned scurl auth is ~a~n" the-remote-scurl)
                  #f])))
  
  ;; used by both the accepting and connecting processes to start and store threads
  ;; monitoring given output and input ports bound to the given canonical host:port
  (define (start-threads/store! i o ra rp)
    (define control-channel (make-async-channel))
    (define ot (run-output-thread o control-channel ra rp))
    (define it (run-input-thread i control-channel ra rp))
    (hash-set! connects-o (cons ra rp) ot)
    (hash-set! connects-i (cons ra rp) it))
  
  ;; -------------------------------------
  
  ;; one thread to manage all tcp-accepts.  
  (define accepter (thread do-accepts))
  ;; one thread to monitor the outgoing messages and redirect them
  (define sendmaster (thread do-sending))
  sendmaster)

(define (request->serialized req)
  (serialize (request-message req)))

;; COMPRESSION - controlled by global
(define (compress msg)
  (define o (open-output-bytes))
  (write msg o)
  (define b# (get-output-bytes o))
  (vector (compress-bytes b#) (bytes-length b#)))

(define (decompress msg)
  (read (open-input-bytes (uncompress-bytes (vector-ref msg 0) (vector-ref msg 1)))))

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

;; SCURL generation and recreation. 

;; right now, just use defaults and don't negotiate.
(define *DIGEST-TYPE* digest:sha512)
(define *KEY-TYPE* pkey:rsa)
(define *KEY-LEN* 2048)

(define (generate-key/defaults)
  (generate-key *KEY-TYPE* *KEY-LEN*))

;; use generate-scurl/defaults when constructing URLs referring to self
(define (generate-scurl/defaults hostname port #:path [path ""] #:key [key (generate-key/defaults)])
  (generate-scurl (format "imp://~a:~a/~a" hostname port path)
                  *DIGEST-TYPE* *KEY-TYPE* key))

;; use triple->scurl/defaults when connecting to a known host:port:PK triple
;; (not useful for generating, as opposed to verifying, since it's never backed by a private key)
(define (triple->scurl/defaults hostname port key)
  (string->scurl (format "imp://~a:~a/scurl/~a" hostname port key) *DIGEST-TYPE* *KEY-TYPE*))