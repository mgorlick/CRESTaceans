#lang racket/base

(require "message.rkt"
         "../net/structs.rkt"
         "../../../Motile/compile.rkt"
         "../../../Motile/serialize.rkt"
         "../../../Motile/baseline.rkt")

(provide (all-defined-out)
         (all-from-out 
          "../../../Motile/compile.rkt"
          "../../../Motile/serialize.rkt"
          "../../../Motile/baseline.rkt"))

;; "client-side"

(define (compile/serialize method request-thread host port key expr [url "/"])
  (define the-compiled-expr (motile/compile expr))
  (define msg (message/ask/new method url the-compiled-expr '()))
  (thread-send request-thread (request host port key msg)))

;; "server-side"

(define-syntax start-program
  (syntax-rules ()
    [(k expr be)
     (let ([fun (motile/start expr be)])
       (cond [(procedure? fun)
              (motile/start* fun be)]
             [else fun]))]
    [(k expr be arg0 ...)
     (let ([fun (motile/start expr be)])
       (cond [(procedure? fun)
              (motile/start* fun be arg0 ...)]
             [else fun]))]))