#! /usr/bin/env racket
#lang racket/base

(require "../src/net/listener.rkt"
         "../../old/Mischief/baseline.rkt"
         "../../old/Mischief/z.rkt"
         "../../old/Mischief/xserialize.rkt"
         "../../old/Mischief/message.rkt"
         "../../old/Mischief/tuple.rkt"
         racket/match
         racket/async-channel)

(define (deserialize/recompile bstr [be BASELINE])
  (deserialize (read (open-input-bytes bstr)) be #f))

(define (start expr . args)
  (define fun (mischief/start expr-bstr))
  (if (equal? (sub1 (procedure-arity fun))
              (length args)) ; first arg is always the continuation k
      (thread (λ () (apply fun (cons rtk/RETURN args))))
      (error (format "Generated code expects a different number of args: ~a" (sub1 (procedure-arity fun))))))

;; enet stuff

(define reply-channel (make-async-channel))
(define request-channel (run-listener "127.16.121.135" 1234 reply-channel))

(let loop ([t 0])
  (match (async-channel-get reply-channel)
    [(response host port data)
     (define message (deserialize/recompile data))
     (match message
       [(vector 'tuple '(mischief message ask) #"SPAWN" an-url body a b c)
        (start body t)]
       [anyelse (printf "~a~n" anyelse)])
     (loop (add1 t))]))