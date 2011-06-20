#! /usr/bin/env racket
#lang racket/base

(require "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/api/compilation.rkt"
         racket/match)

(require "../../Motile/persistent/hash.rkt")

(define request-channel (run-tcp-peer *LOCALHOST* 1235 (current-thread)))

(define (handle-message message t)
  (match message
    [(vector <tuple> '(mischief message ask) #"SPAWN" an-url body a b c)
     (start-program body t)]
    [(vector <tuple> '(mischief message ask) #"POST" an-url name a b c)
     #f]
    [anyelse
     (printf "some other message: ~s~n" anyelse)]))

(let loop ([t 0])
  (handle-message (thread-receive) t)
  (loop (add1 t)))