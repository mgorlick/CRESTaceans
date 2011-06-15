#! /usr/bin/env racket
#lang racket/base

(require "../src/net/tcp-peer.rkt"
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

(define request-channel (run-tcp-peer "128.159.58.146" 1234 (current-thread)))

(define (test)
  (let loop ([t 0])
    (match (thread-receive)
      [(response _ _ data)
       (define message (deserialize/recompile data))
       (match message
         [(vector 'tuple '(mischief message ask) #"SPAWN" an-url body a b c)
          (start-program body t)]
         [(vector 'tuple '(mischief message ask) #"POST" an-url name a b c)
          ;(printf "the gremlin's name is ~a~n" (mischief/start name))
          #f]
         [anyelse (printf "~a~n" anyelse)])
       (loop (add1 t))]
      [else (printf ("no match~n"))])))

(test)