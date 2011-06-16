#! /usr/bin/env racket
#lang racket/base

(require "../src/net/tcp-peer.rkt"
         (only-in "../src/api/compilation.rkt"
                  compile/serialize))

(define port
  (with-handlers ([exn:fail? (λ (e) 5000)])
    (string->number (vector-ref (current-command-line-arguments) 0))))

(define request-thread (run-tcp-peer *LOCALHOST* port (current-thread)))

(define *RHOST* *LOCALHOST*)
(define *RPORT* 1234)

(define the-gremlin 
  `(lambda (t)
     (let loop ([x 1])
       (sleep 0.5)
       (printf "Gremlin number ~a took ~a of your lug nuts~n" t x)
       (loop (add1 x)))))

(define (send-gremlin) (compile/serialize #"SPAWN" request-thread *RHOST* *RPORT* the-gremlin))
(define (post-gremlin-name name) (compile/serialize #"POST" request-thread *RHOST* *RPORT* name))
(define name (make-bytes 10000))

(require profile)
(profile-thunk
 (λ ()
   (let loop ([x 100000])
     ;(sleep 1)
     (compile/serialize #"POST" request-thread *RHOST* *RPORT* the-gremlin)
     (unless (zero? x) (loop (sub1 x))))
   (semaphore-wait (make-semaphore)))
 #:delay 0.005
 #:threads #t)

#|(printf "spawning this program:~n")
(pretty-print the-gremlin)
(printf "~n")

(let loop ()
  (send-gremlin)
  (sleep 1)
  (loop))|#