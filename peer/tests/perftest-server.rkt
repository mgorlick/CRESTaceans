#! /usr/bin/env racket
#lang racket/base

(require "../src/net/tcp-peer.rkt"
         "../src/net/structs.rkt"
         "../src/api/compilation.rkt"
         racket/match)

(define (handle-message message t)
  (match message
    [(vector <tuple> '(mischief message ask) "SPAWN" an-url body a b c)
     #f]
    
    [(vector <tuple> '(mischief message ask) "POST" an-url name a b c)
     #f]
    
    [anyelse #f]))

(define *RHOST* *LOCALHOST*)
(define *RPORT* 5000)
(define *LOCALPORT* 1234)

(require profile)
(profile-thunk
 (λ ()
   (define k (generate-key/defaults))
   (define this-scurl (generate-scurl/defaults *LOCALHOST* *LOCALPORT* #:key k))
   (define request-thread (run-tcp-peer *LOCALHOST* *LOCALPORT* this-scurl (current-thread)))
   (printf "Listening on ~a~n" (scurl->string this-scurl))
   
   (let loop ([t 0])
     (handle-message (thread-receive) t)
     ;(compile/serialize #"POST" request-thread *RHOST* *RPORT* #"OK")
     (loop (add1 t))))
 #:threads #t)