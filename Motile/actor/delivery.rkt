#lang racket/base

(require "curl.rkt")
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

; #(curl? any [curl? if promise version])
(define (delivery? m)
  (and (vector? m) 
       (> (vector-length m) 0) (curl? (vector-ref m 0)) ; slot 0 always a curl.
       (or (= (vector-length m) 2) ; slot 1 anything.
           (and (> (vector-length m) 2) (curl? (vector-ref m 2))) ; if slot 2 exists, always a curl.
           )))

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