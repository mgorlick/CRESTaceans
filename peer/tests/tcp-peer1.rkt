#! /usr/bin/env racket
#lang racket/base

(require "../src/net/tcp-peer.rkt"
         "../../old/Mischief/z.rkt"
         "../../old/Mischief/xserialize.rkt"
         "../../old/Mischief/message.rkt"
         racket/function
         racket/pretty
         racket/port)

(define port
  (with-handlers ([exn:fail? (λ (e) 5000)])
    (string->number (vector-ref (current-command-line-arguments) 0))))

(define request-thread (run-tcp-peer "128.195.59.191" port (current-thread)))

(define (write/send host port msg)
  (define the-serialized-msg (serialize msg))
  (define o (open-output-bytes))
  (write the-serialized-msg o)
  (define bytes (get-output-bytes o))
  (thread-send request-thread (list 'send host port (zip bytes))))

(define (compile/serialize method host port expr)
  (define the-compiled-expr (mischief/compile expr))
  (define msg (message/ask/new method #"/someurl" the-compiled-expr '()))
  (write/send host port msg))

(define compile/serialize/spawn (curry compile/serialize #"SPAWN"))
(define compile/serialize/post (curry compile/serialize #"POST"))

(define *RHOST* "128.195.58.146")
(define *RPORT* 1234)

(define the-gremlin 
  `(lambda (t)
     (let loop ([x 1])
       (sleep 0.5)
       (printf "Gremlin number ~a took ~a of your lug nuts~n" t x)
       (loop (add1 x)))))

(define (send-gremlin) (compile/serialize/spawn *RHOST* *RPORT* the-gremlin))
(define (post-gremlin-name name) (compile/serialize/post *RHOST* *RPORT* name))
(define name (make-bytes 10000))

(define (test-post)
  (let loop ([x 100000])
    ;(sleep 1)
    (post-gremlin-name name)
    (unless (zero? x) (loop (sub1 x)))))

(require profile)
(profile-thunk
 (λ ()
   (test-post))
 ;(semaphore-wait (make-semaphore)))
 #:delay 0.001
 #:threads #t)

#|(printf "spawning this program:~n")
(pretty-print the-gremlin)
(printf "~n")

(let loop ()
  (send-gremlin)
  (sleep 1)
  (loop))|#