#lang racket

(require "persistent_binomial_heap.rkt")

;;Builds a stack of binary heaps (i.e. a history of states) by adding one values from values-list at a time
(define (build-bheap-stack values-list)
  (let ([bheap-stack (list (bheap/new))])
    (map (lambda (value)
           (set! bheap-stack (cons (bheap/insert (car bheap-stack) value value) bheap-stack)))
         values-list)
    bheap-stack))

(define (random-sort list)
  (sort list
        (lambda (x y)
          (equal? 0 (random 2)))))

;;Tests the bheap by building a heap with values added in random order,
;;and then asserting that removing the max one by one matches up with
;;the sorted list of values
(define (test-bheap-random max-value)
  (let* ([values-list (random-sort (build-list max-value values))]
         [sorted-values (sort values-list >)]
         [bheap-stack (build-bheap-stack values-list)])
    (bheap/sanity-check (car bheap-stack))
    (let verify-final ([heap (car bheap-stack)] [values-list sorted-values])
      (if (or (null? values-list) (bheap/empty? heap))
          (printf "Random build test passed!~n")
          (let-values ([(max-priority max-value) (bheap/peek-at-max heap)])
            (if (not (= max-value (car values-list)))
                (begin
                  ;;(printf "Max should be ~s, is actually ~s~n" (car values-list) (cdr (bheap/peek-at-max heap)))
                  (error "Test failed"))
                (begin
                  ;;(printf "Match - max value should be ~s, is ~s~n" (car values-list) (cdr (bheap/peek-at-max heap)))
                  (let-values ([(max-value new-heap) (bheap/remove-max heap)])
                    (verify-final new-heap (cdr values-list))))))))))

;;Tests the persistence of the bheap by building a stack of heaps from inserting
;;elements in order, and then asserting that the max value of each of those
;;heaps in the stack is the expected value
(define (test-bheap-persistence max-value)
  (let* ([values-list (build-list max-value values)]
         [bheap-stack (build-bheap-stack values-list)]
         [inorder-stack (cdr (reverse bheap-stack))])
    (bheap/sanity-check (car bheap-stack))
    (let inorder-verify ([heap-stack inorder-stack] [val-list values-list])
      (if (null? val-list)
          (printf "Persistence test passed!~n")
          (let-values ([(max-priority max-value) (bheap/peek-at-max (car heap-stack))])
            (if (not (= max-value (car val-list)))
                (begin
                  ;;(printf "Max should be ~s, is actually ~s~n" (car val-list) (cdr (bheap/peek-at-max (car heap-stack))))
                  (error "Test failed"))
                (begin
                  ;;(printf "Match - max value should be ~s, is ~s~n" (car val-list) (cdr (bheap/peek-at-max (car heap-stack))))
                  (inorder-verify (cdr heap-stack) (cdr val-list)))))))))

(define (test-bheap-inorder max-value)
  (let ([values-list (build-list max-value values)]
        [bheap (bheap/new)])
    (map (lambda (value)
           (set! bheap (bheap/insert bheap value 1))
           (set! bheap (bheap/insert bheap value 2))
           (set! bheap (bheap/insert bheap value 3)))
         values-list)
    (bheap/sanity-check bheap)
    (dequeue-all-with-inorder-check bheap)
    (printf "Inorder test passed!~n")))

(define (dequeue-all-with-inorder-check bheap)
  (if (bheap/empty? bheap)
      #t
      (let ([index 1])
        (do ([i 1 (add1 i)])
          ([> i 3])
          (let-values ([(max-value new-heap) (bheap/remove-max bheap)])
            (set! bheap new-heap)
            ;;(printf "Value is ~s, expected ~s~n" max-value index)
            (if (not (= max-value index))
                (error "Inorder not preserved")
                (set! index (add1 index)))))
        (dequeue-all-with-inorder-check bheap))))
  
  
  
;;Does both the random insertion and persistence tests
(define (test-bheap-robust max-value)
  (test-bheap-persistence max-value)
  (test-bheap-random max-value)
  (test-bheap-inorder max-value))

;;Benchmark functions below

;;Builds a binary heap containing all the values in values-list
(define (build-bheap bheap values-list)
  (if (null? values-list)
      bheap
      (build-bheap (bheap/insert bheap (car values-list) (car values-list)) (cdr values-list))))

;;Dequeues all the elements in a bheap (performs additional verification by confirming the max value removed is the expected value)
(define (dequeue-all bheap values-list)
  (if (or (bheap/empty? bheap))
      #t
      (let-values ([(max-value new-heap) (bheap/remove-max bheap)])
        (if (not (= max-value (car values-list)))
            (error "UH OH!")
            (dequeue-all new-heap (cdr values-list))))))

;;Builds a heap of values up to max value x-times and returns the average time for building the heaps
(define (bheap-build-benchmark max-value x-times)
  (let ([values-list (random-sort (build-list max-value values))]
        [start-time (current-milliseconds)])
    (do ([i 1 (add1 i)])
      ([> i x-times])
      (build-bheap (bheap/new) values-list))
    (let ([end-time (current-milliseconds)])
      (quotient (- end-time start-time) x-times))))

;;Performes the dequeue-all test x-times and returns the average time for dequeueing all elements from the heap
(define (bheap-dequeue-all-benchmark max-value x-times)
  (let* ([values-list (random-sort (build-list max-value values))]
        [bheap (build-bheap (bheap/new) (remove 0 values-list))]
        [sorted-values (sort values-list >)]
        [start-time (current-milliseconds)])
    (do ([i 1 (add1 i)])
      ([> i x-times])
      (dequeue-all bheap sorted-values))
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

;;Runs all the benchmark tests
(define (all-benchmarks num-trials max-value x-times-per-bm)
  (do-x-benchmarks num-trials 'Build bheap-build-benchmark max-value x-times-per-bm)
  (do-x-benchmarks num-trials 'Remove bheap-dequeue-all-benchmark max-value x-times-per-bm))

(define (run-benchmark-suite)
  (all-benchmarks 5 1000 100)
  (all-benchmarks 5 2000 100)
  (all-benchmarks 5 4000 100)
  (all-benchmarks 5 6000 100)
  (all-benchmarks 5 8000 100)
  (all-benchmarks 5 10000 100))
  ;;(all-benchmarks 5 20000 100))