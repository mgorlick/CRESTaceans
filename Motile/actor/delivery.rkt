#lang racket/base

(provide delivery
         delivery?
         delivery/curl-used
         delivery/contents-sent
         delivery/promise-fulfillment)

; x - subject of type test
; type? - type predicate for x
; where - symbol (typially name of function containing the type assertion)
; expectation - string giving expected type
(define-syntax-rule (assert/type x type? where expectation)
  (when (not (type? x))
    (raise-type-error 'where expectation x)))

(define (delivery? m)
  (and (vector? m)
       (or (= (vector-length m) 2)
           (= (vector-length m) 3))))
(define delivery vector)
(define (delivery/curl-used m)
  (assert/type m vector? 'delivery/curl-used "<curl>")
  (vector-ref m 0))
(define (delivery/contents-sent m)
  (assert/type m vector? 'delivery/contents-sent "<curl>")
  (vector-ref m 1))
(define (delivery/promise-fulfillment m)
  (assert/type m vector? 'delivery/promise-fulfillment "<curl>")
  (and (>= (vector-length m) 3)
       (vector-ref m 2)))