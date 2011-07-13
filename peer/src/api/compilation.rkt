#lang racket/base

(require "message.rkt"
         "../net/structs.rkt"
         "../../../Motile/compile.rkt"
         "../../../Motile/serialize.rkt"
         "../../../Motile/baseline.rkt")

(provide (all-defined-out)
         (all-from-out 
          "message.rkt"
          "../../../Motile/compile.rkt"
          "../../../Motile/serialize.rkt"
          "../../../Motile/baseline.rkt"))

(define (no-return)
  (semaphore-wait (make-semaphore)))

;; "client-side"

(define (ask/send request-thread method url expr
                  #:compile? [compile? #t]
                  #:metadata [metadata :no-metadata:]
                  #:reply [reply :no-reply:]
                  #:echo [echo :no-echo:])
  (define the-compiled-expr (if compile? (motile/compile expr) expr))
  (define msg (message/ask/new method url the-compiled-expr metadata reply echo))
  (thread-send request-thread
               (request (car (:message/uri/authority url))
                        (cdr (:message/uri/authority url))
                        (:message/uri/scheme url) msg)))

;; "server-side"

(define-syntax start-program
  (syntax-rules ()
    [(k expr)
     (start-program expr BASELINE)]
    
    [(k expr be)
     (let ([fun (motile/start expr be)])
       (cond [(procedure? fun) (motile/start fun be)]
             [else fun]))]
    
    [(k expr be arg0 ...)
     (let ([fun (motile/start expr be)])
       (cond [(procedure? fun)
              (motile/start* fun be arg0 ...)]
             [else fun]))]))