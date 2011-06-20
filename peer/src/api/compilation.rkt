#lang racket/base

(require racket/function
         "message.rkt"
         "../net/structs.rkt"
         "../../../Motile/compile.rkt"
          "../../../Motile/serialize.rkt"
          "../../../Motile/baseline.rkt")

(provide (all-defined-out)
         (all-from-out 
          "../../../Motile/compile.rkt"
          "../../../Motile/serialize.rkt"
          "../../../Motile/baseline.rkt"))

(print-graph #f)

;; "client-side"

(define (compile/serialize method request-thread host port expr [url "/"])
  ;(bytes? thread? string? exact-nonnegative-integer? any/c . -> . void)
  (define the-compiled-expr (mischief/compile expr))
  (define msg (message/ask/new method url the-compiled-expr '()))
  (thread-send request-thread (request host port msg)))

;; "server-side"

(define (bytes->message bstr)
  (deserialize (read (open-input-bytes bstr)) BASELINE #f))

(define (start-program expr . args)
  (define fun (mischief/start expr)) ; fun is either a procedure or a Motile primitive
  (if (procedure? fun)
      (if (equal? (sub1 (procedure-arity fun)) (length args)) ; first arg is always the continuation k
          (thread (λ () (apply fun (cons rtk/RETURN args))))
          (error (format "Generated code expects a different number of args: ~a"
                         (sub1 (procedure-arity fun)))))
      fun))