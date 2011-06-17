#lang racket/base

(require racket/function
         "message.rkt"
         "../net/structs.rkt"
         "../../../Motile/compile.rkt"
         "../../../Motile/serialize.rkt"
         "../../../Motile/baseline.rkt")

(provide (all-defined-out))

(print-graph #f)

;; "client-side"

(define (compile/serialize method request-thread host port expr)
  ;(bytes? thread? string? exact-nonnegative-integer? any/c . -> . void)
  (define the-compiled-expr (mischief/compile expr))
  (define msg (message/ask/new method #"/someurl" the-compiled-expr '()))
  (define o (open-output-bytes))
  (write (serialize msg) o)
  (thread-send request-thread (request host port (get-output-bytes o)))
  (close-output-port o))

(define compile/serialize/spawn (curry compile/serialize #"SPAWN"))
(define compile/serialize/post (curry compile/serialize #"POST"))

;; "server-side"

(define (deserialize/recompile bstr [be BASELINE])
  (deserialize (read (open-input-bytes bstr)) be #f))

(define (start-program expr . args)
  (define fun (mischief/start expr))
  (if (equal? (sub1 (procedure-arity fun))
              (length args)) ; first arg is always the continuation k
      (thread (λ () (apply fun (cons rtk/RETURN args))))
      (error (format "Generated code expects a different number of args: ~a"
                     (sub1 (procedure-arity fun))))))