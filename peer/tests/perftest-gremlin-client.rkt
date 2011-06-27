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
   
   (define the-gremlin 
     `(lambda (t)
        (let loop ([x 1])
          (sleep 0.5)
          (printf "Gremlin number ~a took ~a of your lug nuts~n" t x)
          (loop (add1 x)))))
   
   (define (send-gremlin) (compile/serialize #"SPAWN" request-thread *RHOST* *RPORT* the-gremlin))
   
   (let loop ([z 99999])
     (send-gremlin)
     (unless (zero? z) (loop (sub1 z))))
   (semaphore-wait (make-semaphore)))
 #:threads #t)