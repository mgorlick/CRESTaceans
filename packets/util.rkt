#lang typed/racket

(provide (all-defined-out))

(define & bitwise-and)
(define ~ bitwise-not)
(define ^ bitwise-xor)
(define $ bitwise-ior)
(define << arithmetic-shift)
(: >> (Integer Integer -> Integer))
(define (>> n m)
  (arithmetic-shift n (- m)))