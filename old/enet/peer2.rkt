#! /usr/bin/env racket
#lang racket/base

(require "../src/net/listener.rkt"
         "../../old/Mischief/baseline.rkt"
         "../../old/Mischief/z.rkt"
         "../../old/Mischief/xserialize.rkt"
         "../../old/Mischief/message.rkt"
         racket/match)

(define (deserialize/recompile bstr [be BASELINE])
  (deserialize (read (open-input-bytes bstr)) be #f))

(define (start-program expr . args)
  (define fun (mischief/start expr))
  (if (equal? (sub1 (procedure-arity fun))
              (length args)) ; first arg is always the continuation k
      (thread (λ () (apply fun (cons rtk/RETURN args))))
      (error (format "Generated code expects a different number of args: ~a" (sub1 (procedure-arity fun))))))

;; enet stuff

(define request-thread (run-listener "localhost" 1234 (current-thread)))

(let loop ([t 0])
  (match (thread-receive)
    [(response host port data)
     (define message (deserialize/recompile data))
     (match message
       [(vector 'tuple '(mischief message ask) #"SPAWN" an-url body a b c)
        (start-program body t)]
       [(vector 'tuple '(mischief message ask) #"POST" an-url name a b c)
        #f]
       [anyelse (printf "~a~n" anyelse)])
     (loop (add1 t))]))