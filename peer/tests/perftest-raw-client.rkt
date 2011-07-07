#! /usr/bin/env racket
#lang racket/base

(require "../src/net/tcp-peer.rkt"
         "../src/api/compilation.rkt")

(require profile)

(profile-thunk
 (λ ()
   (define *RKEY*
     (with-handlers ([exn:fail? (λ (e) (printf "NO KEY SPECIFIED!~n") #f)])
       (string->bytes/utf-8 (vector-ref (current-command-line-arguments) 0))))
   
   (define port
     (with-handlers ([exn:fail? (λ (e) 5000)])
       (string->number (vector-ref (current-command-line-arguments) 1))))
   
   (define k (generate-key/defaults))
   (define this-scurl (generate-scurl/defaults *LOCALHOST* port #:key k))
   (define request-thread (run-tcp-peer *LOCALHOST* port this-scurl (current-thread)))
   
   
   (define *RHOST* *LOCALHOST*)
   (define *RPORT* 1234)
   
   (define len 10000)
   (define name (make-bytes len))
   (for/list ([i (in-range len)])
     (bytes-set! name i (random 255)))
   
   (let loop ([x 99999])
     (ask/send #"POST" request-thread *RHOST* *RPORT* *RKEY* name #:compile? #f)
     (unless (zero? x) (loop (sub1 x))))
   (semaphore-wait (make-semaphore)))
 
 #:threads #t)