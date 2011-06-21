#lang racket/base

(require racket/tcp
         racket/contract
         racket/port
         racket/function
         racket/match
         racket/async-channel
         "structs.rkt"
         "../api/compilation.rkt")

(print-graph #f)

(provide *LOCALHOST*
         run-tcp-peer)

(define island-pair/c (cons/c string? exact-nonnegative-integer?))
(define portHT/c (hash/c island-pair/c thread?))

(define *LOCALHOST* "127.0.0.1")

(define/contract (run-tcp-peer hostname port reply-thread)
  (string? exact-nonnegative-integer? thread? . -> . thread?)
  
  (define listener (tcp-listen port 64 #f hostname))
  
  ;; connections we've made to long-lived island pairs
  (define connects-o (make-hash)) ; (hash/c island-pair/c thread?)
  (define connects-i (make-hash)) ; (hash/c island-pair/c thread?)
  
  ;; RECEIVING
  (define (run-input-thread i control-channel ra rp)
    (thread
     (λ ()
       (define (delete-self) (hash-remove! connects-i (cons ra rp))
         (printf "inputs: ~a~n" connects-i))
       
       ;(define reade (read-bytes-line-evt i 'return-linefeed))
       (define eofe (eof-evt i))
       (with-handlers ([exn? (λ (e) 
                               (printf "input thread error: ~a~n" (exn-message e))
                               (printf "terminating connection~n")
                               (tcp-abandon-port i)
                               (delete-self)
                               (async-channel-put control-channel 'exit))])
         (let loop ()
           (define v (sync eofe control-channel i)) ; get either an exit signal or a new message every cycle
           (cond [(equal? i v) ; ready to block in order to read something
                  (read-in i reply-thread)
                  (loop)]
                 [(eof-object? v) ; signals termination of connection from other side
                  (raise (make-exn:fail:network "Encountered EOF" (current-continuation-marks)))]
                 [(equal? 'exit v) ; signals termination of connection from same side
                  (raise (make-exn:fail:network "Received close command" (current-continuation-marks)))]))))))
  
  ;;; SENDING
  (define (run-output-thread o control-channel ra rp)
    (thread
     (λ ()
       (define (delete-self) (hash-remove! connects-o (cons ra rp))
         (printf "outputs: ~a~n" connects-o))
       
       (define tre (thread-receive-evt))
       (define tre? (curry equal? tre))
       
       (with-handlers ([exn? (λ (e)
                               (printf "output thread error: ~a~n" (exn-message e))
                               (printf "terminating connection~n")
                               (tcp-abandon-port o)
                               (delete-self)
                               (async-channel-put control-channel 'exit))])
         (let loop ()
           (define v (sync control-channel tre))
           (cond [(tre? v) ; should be a message ready to write
                  (define m (thread-receive))
                  (cond [(list? m) (write-out o m)])
                  (loop)]
                 [(equal? 'exit v) ; signals termination of connection from this side
                  (raise (make-exn:fail:network "Received close command" (current-continuation-marks)))]))))))
  
  ;;; CONNECTING
  (define (do-accepts)
    (let*-values ([(i o) (tcp-accept listener)]
                  [(ra rp) (read-preferred-address i)])
      (printf "accepted from ~a:~a~n" ra rp)
      (start-threads/store! i o ra rp)
      (do-accepts)))
  
  ;; Forward outbound message to the thread managing the appropriate output port.
  (define (do-sending o)
    (file-position o 0)
    (define req (thread-receive))
    (cond [(request? req)
           (define othread (hash-ref connects-o
                                     (cons (request-host req) (request-port req))
                                     (λ () (connect/store! req))))
           (thread-send othread (request->serialized req o) #f)
           (do-sending o)]))
  
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
  (define sendmaster (thread (λ () (do-sending (open-output-bytes)))))
  sendmaster)

;; UTIL
(define number->bytes (compose string->bytes/utf-8 number->string))
(define bytes->number (compose string->number bytes->string/utf-8))

(define (request->serialized req o)
  (serialize (request-message req)))

;; OUTPUT
(define (write-preferred-address o hostname port)
  (write-bytes #"ADDRESS " o)
  (write-bytes (string->bytes/utf-8 hostname) o)
  (write-bytes #" " o)
  (write-bytes (number->bytes port) o)
  (write-bytes #"\r\n" o))

(define (write-out o data)
  (write data o)
  (flush-output o)
  (sendtest (+ 6 799))) ; FIXME

;; INPUT
(define (read-preferred-address i)
  (let ([b (read-bytes-line i 'return-linefeed)])
    (if (bytes? b)
        (match (regexp-split #rx#" " b)
          [(list #"ADDRESS" hostname port#)
           (values (bytes->string/utf-8 hostname) (bytes->number port#))]
          [else (raise 
                 (make-exn:fail:network "Malformed canonical peer address found"(current-continuation-marks)))])
        (raise (make-exn:fail:network "No canonical peer address found" (current-continuation-marks))))))

(define (read-in i reply-thread)
  (thread-send reply-thread (deserialize (read i) BASELINE #f)))

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