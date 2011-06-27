#lang racket/base

(require racket/tcp
         racket/contract
         racket/port
         racket/function
         racket/match
         racket/async-channel
         (planet "main.rkt" ("soegaard" "gzip.plt" 2 2))
         "structs.rkt"
         "../api/compilation.rkt")

(provide *LOCALHOST*
         run-tcp-peer)

(define *USE-COMPRESSION?* #f)

(print-graph #f)

(define island-pair/c (cons/c string? exact-nonnegative-integer?))
(define portHT/c (hash/c island-pair/c thread?))

(define *LOCALHOST* "127.0.0.1")

(define/contract (run-tcp-peer hostname port reply-thread)
  (string? exact-nonnegative-integer? thread? . -> . thread?)
  
  (define listener (tcp-listen port 64 #f hostname))
  
  (define connects-o (make-hash)) ; (hash/c island-pair/c thread?)
  (define connects-i (make-hash)) ; (hash/c island-pair/c thread?)
  
  (define (make-abandon/signal port control-channel self-key)
    (λ (e)
      (printf "~a thread error: ~a~n" (if (input-port? port) "Input" "Output") (exn-message e))
      (printf "terminating connection~n")
      (tcp-abandon-port port)
      (if (input-port? port)
          (hash-remove! connects-i self-key)
          (hash-remove! connects-o self-key))
      (async-channel-put control-channel 'exit)))
  
  ;; RECEIVING
  
  ;; called if the input port gets an eof in input stream.
  (define (done/eof v)
    (when (eof-object? v)
      (raise (make-exn:fail:network "Encountered EOF" (current-continuation-marks)))))
  
  ;; called if input or output thread gets a signal through their distinguished
  ;; control async channel. typically one will signal the other.
  ;; (in the future, may need a third thread to share the ref)
  (define (done/signalled v)
    (when (equal? 'exit v)
      (raise (make-exn:fail:network "Received close command" (current-continuation-marks)))))
  
  (define (run-input-thread i control-channel ra rp)
    (thread
     (λ ()
       (with-handlers ([exn? (make-abandon/signal i control-channel (cons ra rp))])
         (define eofe (handle-evt (eof-evt i) done/eof))
         (define sige (handle-evt control-channel done/signalled))
         (define reade (handle-evt i (curry read-in/forward-message reply-thread)))
         (let loop ()
           (sync eofe sige reade)
           (loop))))))
  
  ;;; SENDING
  
  ;; called if output thread receives anything in mailbox.
  (define (output-next o)
    (define m (thread-receive))
    (cond [(list? m) (write-out o m)]))
  
  (define (run-output-thread o control-channel ra rp)
    (thread
     (λ ()
       (with-handlers ([exn? (make-abandon/signal o control-channel (cons ra rp))])
         (define msge (handle-evt (thread-receive-evt) (λ (v) (output-next o))))
         (define sige (handle-evt control-channel done/signalled))
         (let loop ()
           (sync msge sige)
           (loop))))))
  
  ;;; CONNECTING
  (define (do-accepts)
    (let*-values ([(i o) (tcp-accept listener)]
                  [(ra rp) (read-preferred-address i)])
      (printf "accepted from ~a:~a~n" ra rp)
      (start-threads/store! i o ra rp)
      (do-accepts)))
  
  ;; Forward outbound message to the thread managing the appropriate output port.
  (define (do-sending)
    (define req (thread-receive))
    (cond [(request? req)
           (define othread (hash-ref connects-o (cons (request-host req) (request-port req))
                                     (λ () (connect/store! req))))
           (thread-send othread (request->serialized req) #f)
           (do-sending)]))
  
  (define (connect/store! req)
    (let*-values ([(i o) (tcp-connect (request-host req) (request-port req))]
                  [(la lp ra rp) (tcp-addresses i #t)])
      (write-preferred-address o la port)
      (start-threads/store! i o ra rp)
      (hash-ref connects-o (cons ra rp))))
  
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

;; on connection startup the peer acting in "client" role writes preferred address
;; (i.e., the host/port which its own URLs use to name it)
(define (read-preferred-address i)
  (match (read i)
    [(vector #"ADDRESS" hostname #"PORT" port)
     (values hostname port)]
    [else (raise 
           (make-exn:fail:network 
            "Malformed canonical peer address found" (current-continuation-marks)))]))

;; UTIL
(define number->bytes (compose string->bytes/utf-8 number->string))
(define bytes->number (compose string->number bytes->string/utf-8))

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
(define (write-preferred-address o hostname port)
  (write (vector #"ADDRESS" hostname #"PORT" port) o)
  (flush-output o))

(define (write-w/-compression msg o) (write (compress msg) o))
(define (write-w/o-compression msg o) (write msg o))
(define writer (if *USE-COMPRESSION?*
                   write-w/-compression
                   write-w/o-compression))
(define (write-out o msg)
  (writer msg o)
  (flush-output o)
  (sendtest 0))

;; INPUT
(define reader (if *USE-COMPRESSION?* (compose decompress read) read))
(define (read-in/forward-message reply-thread i)
  (thread-send reply-thread (deserialize (reader i) BASELINE #f)))

;; FOR TESTING ONLY
(define scl (make-semaphore 1))
(define rcl (make-semaphore 1))
(define recvcounter #f) (define recvsize #f)
(define sendcounter #f) (define sendsize #f)
(define start-time #f)

(define (sendtest size)
  (unless sendcounter (init-test!))
  (semaphore-wait scl)
  (set! sendcounter (add1 sendcounter))
  (set! sendsize (+ sendsize size))
  (semaphore-post scl)
  (when (= 0 (modulo sendcounter 100))
    (printf "sent ~a KB | ~a messages in ~a seconds (~a KB/sec | ~a messages/sec)~n"
            (exact->inexact (/ sendsize 1024))
            sendcounter
            (/ (- (current-inexact-milliseconds) start-time) 1000)
            (/ (/ sendsize 1024) (/ (- (current-inexact-milliseconds) start-time) 1000))
            (/ sendcounter (/ (- (current-inexact-milliseconds) start-time) 1000)))))

(define (recvtest size)
  (unless recvcounter (init-test!))
  (semaphore-wait rcl)
  (set! recvcounter (add1 recvcounter))
  (set! recvsize (+ recvsize size))
  (semaphore-post rcl)
  (when (= 0 (modulo recvcounter 100))
    (printf "recved ~a KB | ~a messages in ~a seconds (~a KB/sec | ~a messages/sec)~n"
            (exact->inexact (/ recvsize 1024))
            recvcounter
            (/ (- (current-inexact-milliseconds) start-time) 1000)
            (/ (/ recvsize 1024) (/ (- (current-inexact-milliseconds) start-time) 1000))
            (/ recvcounter (/ (- (current-inexact-milliseconds) start-time) 1000)))))

(define (init-test!)
  (set! sendcounter 0)
  (set! recvcounter 0)
  (set! recvsize 0)
  (set! sendsize 0)
  (set! start-time (current-inexact-milliseconds)))