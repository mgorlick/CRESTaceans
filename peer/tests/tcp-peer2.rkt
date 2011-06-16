#! /usr/bin/env racket
#lang racket/base

(require "../src/net/tcp-peer.rkt"
         "../src/compilation.rkt"
         racket/match)

(define request-channel (run-tcp-peer *LOCALHOST* 1234 (current-thread)))

(define (handle-message message t)
  (match message
    [(vector 'tuple '(mischief message ask) #"SPAWN" an-url body a b c)
     ;(printf "starting a program~n")
     (start-program body t)]
    
    [(vector 'tuple '(mischief message ask) #"POST" an-url name a b c)
     ;(printf "the gremlin's name is ~a~n" (mischief/start name))
     #f]
    
    [anyelse (printf "~some other message: ~n" anyelse)]))

(define (test)
  (let loop ([t 0])
    (match (thread-receive)
      [(struct response (data))
       (define message
         (with-handlers ([exn:fail? (λ (e) #f)])
           (deserialize/recompile data)))
       (if message
           (handle-message message t)
           (printf "error: couldn't deserialize/recompile~n"))
       (loop (add1 t))]
      [else (printf ("no match~n"))])))

(require profile)
(profile-thunk
 test
 #:threads #t
 #:delay 0.005)