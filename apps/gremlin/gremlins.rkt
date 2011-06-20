#! /usr/bin/env racket
#lang racket/base

(require "../../peer/src/net/tcp-peer.rkt"
         (only-in "../../peer/src/api/compilation.rkt"
                  compile/serialize))

(define port
  (with-handlers ([exn:fail? (λ (e) 5000)])
    (string->number (vector-ref (current-command-line-arguments) 0))))

(define request-thread (run-tcp-peer *LOCALHOST* port (current-thread)))

(define *RHOST* *LOCALHOST*)
(define *RPORT* 1235)

(define the-gremlin 
  `(lambda (t)
     (let loop ([x 1])
       (sleep 0.5)
       (printf "Gremlin number ~a took ~a of your lug nuts~n" t x)
       (loop (add1 x)))))

(let loop ()
  (sleep 1)
  (compile/serialize #"SPAWN" request-thread *RHOST* *RPORT* the-gremlin)
  (loop))