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

(define (bytes->message bstr)
  (deserialize (read (open-input-bytes bstr)) BASELINE #f))

(define (start-program expr #:be [be BASELINE] . program-args)
  (define fun (motile/start expr be))
  (cond [(procedure? fun)
         (apply fun (cons rtk/RETURN (cons '#(#f) (cons be program-args))))]
        [else fun]))