#lang racket/base

(require racket/port
         racket/contract
         racket/function
         "message.rkt"
         "../../../Motile/compile.rkt"
         "../../../Motile/serialize.rkt"
         "../../../Motile/baseline.rkt")

(provide (all-defined-out))

(print-graph #f)

(define (compile/serialize method request-thread host port expr)
  ;(bytes? thread? string? exact-nonnegative-integer? any/c . -> . void)
  (define the-compiled-expr (mischief/compile expr))
  (define msg (message/ask/new method #"/someurl" the-compiled-expr '()))
  (thread-send request-thread
               (list 'send host port (with-output-to-bytes
                                      (λ () (write (serialize msg)))))))

(define compile/serialize/spawn (curry compile/serialize #"SPAWN"))
(define compile/serialize/post (curry compile/serialize #"POST"))

(define (deserialize/recompile bstr [be BASELINE])
  (deserialize (read (open-input-bytes bstr)) be #f))

(define (start-program expr . args)
  (define fun (mischief/start expr))
  (if (equal? (sub1 (procedure-arity fun))
              (length args)) ; first arg is always the continuation k
      (thread (λ () (apply fun (cons rtk/RETURN args))))
      (error (format "Generated code expects a different number of args: ~a"
                     (sub1 (procedure-arity fun))))))