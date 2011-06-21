#! /usr/bin/env racket
#lang racket/base

(require "../src/net/tcp-peer.rkt"
         (only-in "../src/api/compilation.rkt"
                  compile/serialize))

(require profile)

(profile-thunk
 (λ ()
   (define port
     (with-handlers ([exn:fail? (λ (e) 5000)])
       (string->number (vector-ref (current-command-line-arguments) 0))))
   
   (define request-thread (run-tcp-peer *LOCALHOST* port (current-thread)))
   
   (define *RHOST* *LOCALHOST*)
   (define *RPORT* 1234)
   
   (define name (make-bytes 10000))
   
   (thread (λ ()
             (let loop ()
               (thread-receive)
               (loop))))
   
   (let loop ([x 100000])
     ;(sleep 1)
     (compile/serialize #"POST" request-thread *RHOST* *RPORT* name)
     (unless (zero? x) (loop (sub1 x))))
   (semaphore-wait (make-semaphore)))
 #:threads #t)

#|(printf "spawning this program:~n")
(pretty-print the-gremlin)
(printf "~n")

(let loop ()
  (send-gremlin)
  (sleep 1)
  (loop))|#