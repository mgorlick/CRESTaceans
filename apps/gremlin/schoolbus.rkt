#! /usr/bin/env racket
#lang racket/base

(require "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/api/compilation.rkt"
         racket/match)

(require "../../Motile/persistent/hash.rkt")

(define SCHOOLBUS
  (pairs/hash
   ENVIRON/TEST
   (list (define/global/1 'sleep sleep)
         (define/global/N 'printf printf))))

(define *RHOST* *LOCALHOST*)
(define *RPORT* 5000)

(define this-scurl (generate-scurl/defaults *LOCALHOST* 1235))
(define request-thread (run-tcp-peer *LOCALHOST* 1235 this-scurl (current-thread)))
(printf "Listening on ~a~n" (scurl->string this-scurl))

(define (handle-message message t)
  (match message
    [(vector <tuple> '(mischief message ask) #"SPAWN" an-url body a b c)
     (start-program body #:be SCHOOLBUS t)]
    [(vector <tuple> '(mischief message ask) #"POST" an-url name a b c)
     #f]
    [anyelse
     (printf "some other message: ~s~n" anyelse)]))

(let loop ([t 0])
  (handle-message (thread-receive) t)
  (loop (add1 t)))