#lang racket

(require "bankers_queue.rkt")

(define (build-queue-stack max-value)
  (let ([stack (list (new))])
    (do ([i 1 (add1 i)])
      ([<= i max-value])
      (set! stack (cons (enqueue (car stack) i) stack)))
    stack))

(define (check-persistence stack value)
  (if (or (null? stack) (empty? (car stack)))
      #t
      (if (not (= value (peek (car stack))))
          #f
          (check-persistence (cdr stack) (sub1 value)))))

(define (dequeue-all-with-value-check q value)
  (if (empty? q)
      #t
      (if (not (= (peek q) value))
          #f
          (dequeue-all-with-value-check (dequeue q) (sub1 value)))))

(define (dequeue-all q)
  (if (empty? q)
      #t
      (dequeue-all (dequeue q))))


(define (test max-value)
  (let ([stack (build-queue-stack max-value)])
    (if (not (check-persistence stack max-value))
        (error "Persistence test failed")
        #t)
    (if (not (dequeue-all-with-value-check (car stack) max-value))
        (error "Dequeue all test failed")
        #t)))

(define (build-queue q values-list)
  (if (null? values-list)
      q
      (build-queue (enqueue q (car values-list)) (cdr values-list))))

(define (enqueue-benchmark max-value x-times)
  (let* ([values-list (build-list max-value values)]
         [start-time (current-milliseconds)])
    (do ([i 1 (add1 i)])
      ([> i x-times])
      (build-queue (new) values-list))
    (let ([end-time (current-milliseconds)])
      (quotient (- end-time start-time) x-times))))

(define (dequeue-benchmark max-value x-times)
  (let* ([values-list (build-list max-value values)]
         [queue (build-queue (new) values-list)]
         [start-time (current-milliseconds)])
    (do ([i 1 (add1 i)])
      ([> i x-times])
      (dequeue-all queue))
    (let ([end-time (current-milliseconds)])
      (quotient (- end-time start-time) x-times))))

;;Helper function for calculating averages
(define (list-sum list)
  (if (null? list)
      0
      (+ (car list) (list-sum (cdr list)))))

;;Does a benchmark test x times, reporting the average of the averages
(define (do-x-benchmarks x id bm-func max-value x-times-per-bm)
  (let ([data '()])
    (do ([i 1 (add1 i)])
      ([> i x])
      (let ([curr-avg (bm-func max-value x-times-per-bm)])
        (set! data (cons curr-avg data))))
    (set! data (reverse data))
    (do ([i 0 (add1 i)])
      ([>= i x])
      (printf "~s ~s average: ~s milliseconds~n" id (add1 i) (list-ref data i)))
    (printf "Overall ~s average: ~s milliseconds~n~n" id (quotient (list-sum data) x))))

(define (all-benchmarks num-trials max-value x-times-per-bm)
  (do-x-benchmarks num-trials 'Enqueue enqueue-benchmark max-value x-times-per-bm)
  (do-x-benchmarks num-trials 'Dequeue enqueue-benchmark max-value x-times-per-bm))

(define (benchmark-suite)
  (all-benchmarks 5 5000 100)
  (all-benchmarks 5 10000 100)
  (all-benchmarks 5 20000 100)
  (all-benchmarks 5 30000 100)
  (all-benchmarks 5 50000 100))
  ;;(all-benchmarks 5 100000 100)
  ;;(all-benchmarks 5 200000 100))