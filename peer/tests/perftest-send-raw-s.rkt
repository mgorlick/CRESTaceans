#! /usr/bin/env racket
#lang racket/base

(require "../src/net/tcp-peer.rkt"
         "../src/net/structs.rkt"
         "../src/api/compilation.rkt"
         racket/match)


(define (handle-message message t)
  (match message
    [(vector <tuple> '(mischief message ask) #"SPAWN" an-url body a b c)
     ;(printf "starting a program~n")
     (start-program body t)]
    
    [(vector <tuple> '(mischief message ask) #"POST" an-url name a b c)
     ;(printf "the gremlin's name is ~a~n" (mischief/start name))
     #f]
    
    [anyelse (printf "some other message: ~s~n" anyelse)]))

(define *RHOST* *LOCALHOST*)
(define *RPORT* 5000)

(require profile)
(profile-thunk
 (λ ()
   (define request-thread (run-tcp-peer *LOCALHOST* 1234 (current-thread)))
   
   (let loop ([t 0])
     (handle-message (thread-receive) t)
     (compile/serialize #"POST" request-thread *RHOST* *RPORT* #"OK")
     (loop (add1 t))))
 #:threads #t)