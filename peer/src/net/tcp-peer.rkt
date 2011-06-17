#lang racket/base

(require racket/tcp
         racket/match
         racket/contract
         racket/port
         racket/function
         "structs.rkt")

(provide (all-defined-out))

(define *LOCALHOST* "::1")

(define (write-out o data)
  (write-bytes (integer->integer-bytes (bytes-length data) 4 #f #t) o)
  (write-bytes #"\r\n" o)
  (write-bytes data o)
  (flush-output o))

(define (read-in i len reply-thread)
  (define message (read-bytes (integer-bytes->integer len #f #t 0 4) i))
  (thread-send reply-thread (response message)))

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
         (match (sync reade tre)
           [(? tre? a)
            (if (equal? (thread-receive) 'exit)
                (close-input-port i)
                (loop))]
           
           [(? bytes? len)
            (read-in i len reply-thread)
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
  
  
  ;;; SENDING
  (define/contract (run-output-thread o)
    (output-port? . -> . thread?)
    (thread
     (λ ()
       (let loop ()
         (match (thread-receive)
           [(? request? v)
            (write-out o (request-data v))
            (sendtest)
            (loop)]
           
           ['exit
            (close-output-port o)])))))
  
  ;; Forward outbound message to the thread managing the appropriate output port.
  (define (do-sending)
    (match (thread-receive)
      [(? request? req)
       (match (hash-ref connects-o (cons (request-host req) (request-port req)) void)
         [(? thread? othread) (thread-send othread req)]
         [(? void? _) (connect/send req)])])
    (do-sending))
  
  (define (connect/send req)
    (let*-values ([(i o) (tcp-connect (request-host req) (request-port req))]
                  [(la lp ra rp) (tcp-addresses i #t)])
      (printf "connected to ~a:~a~n" ra rp)
      (define ot (run-output-thread o))
      (define it (run-input-thread i))
      (hash-set! connects-o (cons ra rp) ot)
      (hash-set! connects-i (cons ra rp) it)
      (thread-send ot req)))
  
  ;; -------------------------------------
  
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