#lang racket/base

(require racket/tcp
         racket/contract
         racket/port
         racket/function
         "structs.rkt"
         "../api/compilation.rkt")

(provide (all-defined-out))

(define *LOCALHOST* "::1")

(define/contract (run-tcp-peer hostname port reply-thread)
  (string? exact-nonnegative-integer? thread? . -> . thread?)
  
  (define listener (tcp-listen port 4 #f hostname))
  (define island-pair/c (cons/c string? exact-nonnegative-integer?))
  (define portHT/c (hash/c island-pair/c thread?))
  
  ;; ephemeral client connections to our long-lived island pair
  (define/contract accepts-o portHT/c (make-hash))
  (define/contract accepts-i portHT/c (make-hash))
  
  ;; connections we've made to long-lived island pairs
  (define/contract connects-o portHT/c (make-hash))
  (define/contract connects-i portHT/c (make-hash))
    
  ;; RECEIVING
  (define/contract (run-input-thread i)
    (input-port? . -> . thread?)
    (thread
     (λ ()
       (define tre (thread-receive-evt))
       (define tre? (curry equal? tre))
       (define reade (read-bytes-line-evt i 'return-linefeed))
       (let loop ()
         (define v (sync reade tre)) ; get either an exit signal or a new message every cycle
         (cond [(bytes? v) ; v is a byte-encoded integer providing framing info
                (read-in i v reply-thread)
                (loop)]
               [(eof-object? v) ; v signals termination of connection
                (close-input-port i)]
               [(tre? v) ; someone signalled the thread
                (if (equal? (thread-receive) 'exit)
                    (close-input-port i)
                    (loop))])))))
  
  ;;; SENDING
  (define/contract (run-output-thread o)
    (output-port? . -> . thread?)
    (thread
     (λ ()
       (let loop ()
         (define v (thread-receive))
         (cond [(bytes? v)
                (write-out o v)
                (loop)]
               [(equal? 'exit v)
                (close-output-port o)])))))
  
  (define (request->serialized req)
    (define o (open-output-bytes))
    (write (serialize (request-message req)) o)
    (get-output-bytes o))
  
  ;;; CONNECTING
  (define (do-accepts)
    (let*-values ([(i o) (tcp-accept listener)]
                  [(la lp ra rp) (tcp-addresses i #t)])
      (printf "accepted from ~a:~a~n" ra rp)
      (hash-set! connects-o (cons ra rp) (run-output-thread o))
      (hash-set! connects-i (cons ra rp) (run-input-thread i)))
    (do-accepts))
  
  ;; Forward outbound message to the thread managing the appropriate output port.
  (define (do-sending)
    (define req (thread-receive))
    (cond [(request? req)
           (define othread (hash-ref connects-o 
                                     (cons (request-host req) (request-port req))
                                     (λ () (connect/store/send! req))))
           (thread-send othread (request->serialized req))
           (do-sending)]))
  
  (define (connect/store/send! req)
    (let*-values ([(i o) (tcp-connect (request-host req) (request-port req))]
                  [(la lp ra rp) (tcp-addresses i #t)])
      (printf "connected to ~a:~a~n" ra rp)
      (define ot (run-output-thread o))
      (define it (run-input-thread i))
      (hash-set! connects-o (cons ra rp) ot)
      (hash-set! connects-i (cons ra rp) it)
      ot))
  
  ;; -------------------------------------
  
  ;; one thread to manage all tcp-accepts.  
  (define accepter (thread do-accepts))
  ;; one thread to monitor the outgoing messages and redirect them
  (define sendmaster (thread do-sending))
  sendmaster)

(define/contract (write-out o data)
  (output-port? bytes? . -> . void)
  (write-bytes (integer->integer-bytes (bytes-length data) 4 #f #t) o)
  (write-bytes #"\r\n" o)
  (write-bytes data o)
  (flush-output o)
  (sendtest (+ 6 (bytes-length data))))

(define/contract (read-in i len# reply-thread)
  (input-port? bytes? thread? . -> . void)
  (thread-send reply-thread (bytes->message (read-message len# i))))

(define (read-message len# i)
  (define m (read-bytes (integer-bytes->integer len# #f #t) i))
  (recvtest (bytes-length m))
  m)

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