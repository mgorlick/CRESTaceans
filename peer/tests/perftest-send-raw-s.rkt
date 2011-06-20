#! /usr/bin/env racket
#lang racket/base

(require "../src/net/tcp-peer.rkt"
         "../src/net/structs.rkt"
         "../src/api/compilation.rkt"
         racket/match)

(define request-channel (run-tcp-peer *LOCALHOST* 1234 (current-thread)))

(define (handle-message message t)
  (match message
    [(vector <tuple> '(mischief message ask) #"SPAWN" an-url body a b c)
     ;(printf "starting a program~n")
     (start-program body t)]
    
    [(vector <tuple> '(mischief message ask) #"POST" an-url name a b c)
     ;(printf "the gremlin's name is ~a~n" (mischief/start name))
     #f]
    
    [anyelse (printf "some other message: ~s~n" anyelse)]))

(require profile)
(profile-thunk
 (λ ()
   (let loop ([t 0])
     (handle-message (thread-receive) t)
     (loop (add1 t))))
 #:threads #t)