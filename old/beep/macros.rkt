#lang racket

(define-syntax (define/public/contract stx)
  (syntax-case stx ()
    [(_ (name args ...) contract body ...)
     (with-syntax ([(arglist ...) (generate-temporaries #'(args ...))])
       #'(begin
           (define/public (name args ...)
             (define/contract (newname arglist ...)
               contract
               body ...)
             (newname args ...))))]))