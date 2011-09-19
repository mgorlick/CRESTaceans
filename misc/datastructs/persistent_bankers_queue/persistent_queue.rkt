#lang racket

(require "persistent_vector.rkt")

(provide
 new
 empty?
 peek
 enqueue
 dequeue
 size)

(define-syntax-rule (access q index)
  (list-ref q index))

(define (front q)
  (access q 0))

(define (rear q)
  (access q 1))

(define (new-with-params front rear)
  (list front rear 'qpersist))

(define (empty)
  (new-with-params '() vector/null))

(define (new)
  (empty))

(define (empty? q)
  (null? (front q)))

(define (qpersist? q)
  (and
   (list? q)
   (= (list-length q) 3)
   (= (access q 2) 'qpersist)))

(define (peek q)
  (car (front q)))

(define (enqueue q obj)
  (if (empty? q)
      (new (list obj) vector/null)
      (new (front q) (vector/cons (rear q) obj))))

(define (dequeue q obj)
  (if (empty? q)
      #f
      (let* ([f1 (cdr (front q))]
             [r1 (rear q)])
        (if (null? f1)
            (new (vector/list r1) vector/null)
            (new f1 r1)))))

(define (size q)
  (+ (length (front q) (vector/length (rear q)))))