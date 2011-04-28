#lang racket

(provide (all-defined-out))

(define-syntax define/match
  (syntax-rules ()
    [(_ (id name) clause ...)
     (define (id name)
       (match name clause ...))]))

(define-syntax define/match*
  (syntax-rules ()
    [(_ (id name ...) clause ...)
     (define (id name ...)
       (match* (name ...)
         clause ...))]))