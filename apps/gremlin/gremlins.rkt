#! /usr/bin/env racket
#lang racket/base

(require "../../peer/src/net/tcp-peer.rkt"
         (only-in "../../peer/src/api/compilation.rkt"
                  compile/serialize))

(define *RKEY*
  (with-handlers ([exn:fail? (λ (e) (printf "NO KEY SPECIFIED!~n") #f)])
    (string->bytes/utf-8 (vector-ref (current-command-line-arguments) 0))))

(define port
  (with-handlers ([exn:fail? (λ (e) 5000)])
    (string->number (vector-ref (current-command-line-arguments) 1))))

(define this-scurl (generate-scurl/defaults *LOCALHOST* port))
(define request-thread (run-tcp-peer *LOCALHOST* port this-scurl (current-thread)))

(define *RHOST* *LOCALHOST*)
(define *RPORT* 1235)

(define the-gremlin 
  `(lambda (t)
     (let loop ([x 1])
       (sleep 0.5)
       (display (format "Gremlin number ~a took ~a of your lug nuts~n" t x))
       (loop (add1 x) ))))

(let loop ()
  (sleep 1)
  (compile/serialize #"SPAWN" request-thread *RHOST* *RPORT* *RKEY* the-gremlin)
  (loop))