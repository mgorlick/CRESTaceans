#lang racket/base


(require "../../Motile/compile/compile.rkt"
         "../../Motile/generate/baseline.rkt"
         "../../Motile/baseline.rkt")
(provide (all-defined-out)
         (all-from-out "../../Motile/compile/compile.rkt"
                       "../../Motile/generate/baseline.rkt"
                       "../../Motile/baseline.rkt"))

(define (eval-definition e)
  (motile/call e BASELINE))
(define-syntax-rule (define-motile-procedure id body)
  (define id (eval-definition (motile/compile body))))