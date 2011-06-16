#lang racket/base

(require racket/tcp
         racket/match
         racket/contract
         racket/port
         racket/function)

(provide (all-defined-out))

(define *LOCALHOST* "::1")

(struct response (data))

(define/contract (run-tcp-peer hostname port reply-thread)
  (string? exact-nonnegative-integer? thread? . -> . thread?)
  
  (define listener (tcp-listen port 4 #f hostname))
  (define island-pair/c (cons/c string? exact-nonnegative-integer?))
  (define portHT/c (hash/c island-pair/c thread?))
  ;; ephemeral client connections to our long-lived island pair
  (define accepts-o (make-hash))
  (define accepts-i (make-hash))
  
  ;; connections we've made to long-lived island pairs
  (define connects-o (make-hash))
  (define connects-i (make-hash))
  
  ;; two threads per tcp-accept
  (define/contract (run-output-thread o)
    (output-port? . -> . thread?)
    (thread
     (λ ()
       (let loop ()
         (match (thread-receive)
           [(list 'send host port (? bytes? data))
            (write-bytes (integer->integer-bytes (bytes-length data) 4 #f #t) o)
            (write-bytes #"\r\n" o)
            (write-bytes data o)
            (flush-output o)
            (sendtest)
            (loop)]
           ['exit
            (close-output-port o)])))))
  
  (define/contract (run-input-thread i)
    (input-port? . -> . thread?)
    (thread
     (λ ()
       (define tre (thread-receive-evt))
       (define reade (read-bytes-line-evt i 'return-linefeed))
       (let loop ()
         (match (sync reade tre)
           [(? (curry equal? tre) a) 
            (if (equal? (thread-receive) 'exit)
                (close-input-port i)
                (loop))]
           [(? bytes? b)
            (define message (read-bytes (integer-bytes->integer b #f #t 0 4) i))
            (thread-send reply-thread (response message))
            (recvtest)
            (loop)]
           [(? (curry equal? eof) e)
            (printf "input port dying~n")
            (close-input-port i)])))))
  
  
  (define (do-accepts)
    (let*-values ([(i o) (tcp-accept listener)]
                  [(la lp ra rp) (tcp-addresses i #t)])
      (printf "accepted from ~a:~a~n" ra rp)
      (hash-set! connects-o (cons ra rp) (run-output-thread o))
      (hash-set! connects-i (cons ra rp) (run-input-thread i)))
    (do-accepts))
  
  (define (do-sending)
    (match (thread-receive)
      [(list 'send host port data)
       (let ([othread (hash-ref connects-o (cons host port) (λ () #f))])
         (if othread
             (thread-send othread (list 'send host port data))
             (connect/send host port data)))])
    (do-sending))
  
  (define/contract (connect/send host port data)
    (string? exact-nonnegative-integer? (or/c bytes? list?) . -> . void)
    (let*-values ([(i o) (tcp-connect host port)]
                  [(la lp ra rp) (tcp-addresses i #t)])
      (printf "connected to ~a:~a~n" ra rp)
      (define ot (run-output-thread o))
      (define it (run-input-thread i))
      (hash-set! connects-o (cons ra rp) ot)
      (hash-set! connects-i (cons ra rp) it)
      (thread-send ot (list 'send host port data))))
  
  ;; one thread to manage all tcp-accepts.  
  (define accepter (thread do-accepts))
  ;; one thread to monitor the outgoing messages and redirect them
  (define sendmaster (thread do-sending))
  sendmaster)

(require file/gzip
         file/gunzip)

(define (zip data)
  (define oz (open-output-bytes))
  (deflate (open-input-bytes data) oz)
  (get-output-bytes oz))

(define (unzip data)
  (define oz (open-output-bytes))
  (inflate (open-input-bytes data) oz)
  (get-output-bytes oz))

;; FOR TESTING ONLY
(define scl (make-semaphore 1))
(define rcl (make-semaphore 1))
(define recvcounter #f)
(define sendcounter #f)
(define start-time #f)

(define (sendtest)
  (unless sendcounter (init-test!))
  (semaphore-wait scl)
  (set! sendcounter (add1 sendcounter))
  (semaphore-post scl)
  (when (= 0 (modulo sendcounter 100))
    (collect-garbage)
    (printf "sent ~a messages in ~a seconds (~a messages/sec)~n"
            sendcounter
            (/ (- (current-inexact-milliseconds) start-time) 1000)
            (/ sendcounter (/ (- (current-inexact-milliseconds) start-time) 1000)))))

(define (recvtest)
  (unless recvcounter (init-test!))
  (semaphore-wait rcl)
  (set! recvcounter (add1 recvcounter))
  (semaphore-post rcl)
  (when (= 0 (modulo recvcounter 100))
    (collect-garbage)
    (printf "recved ~a messages in ~a seconds (~a messages/sec)~n"
            recvcounter
            (/ (- (current-inexact-milliseconds) start-time) 1000)
            (/ recvcounter (/ (- (current-inexact-milliseconds) start-time) 1000)))))

(define (init-test!)
  (set! sendcounter 0)
  (set! recvcounter 0)
  (set! start-time (current-inexact-milliseconds)))