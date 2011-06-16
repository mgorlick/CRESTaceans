#! /usr/bin/env racket
#lang racket/base

(require "../src/net/listener.rkt"
         "../../old/Mischief/z.rkt"
         "../../old/Mischief/xserialize.rkt"
         "../../old/Mischief/message.rkt")

;; enet stuff
(define port
  (with-handlers ([exn:fail? (λ (e) 5000)])
    (string->number (vector-ref (current-command-line-arguments) 0))))

(define request-thread (run-listener "localhost" port (current-thread)))



(define (compile/serialize/spawn host port expr)
  (define o (open-output-bytes))
  (define msg (message/ask/new #"SPAWN" #"/someurl" (mischief/compile expr) '()))
  (write (serialize msg) o)
  (thread-send request-thread (list 'send host port (get-output-bytes o))))

(define (compile/serialize/post host port expr)
  (define o (open-output-bytes))
  (define msg (message/ask/new #"POST" #"/someurl" (mischief/compile expr) '()))
  (write (serialize msg) o)
  (thread-send request-thread (list 'send host port (get-output-bytes o))))

(define *RHOST* "localhost")
(define *RPORT* 1234)

(define (send-gremlin)
  (define z 1)
  (compile/serialize/spawn
   *RHOST* *RPORT*
   `(lambda (t)
      (let loop ([x ,z])
        (sleep 0.5)
        (printf "Gremlin number ~a took ~a of your lug nuts~n" t x)
        (loop (add1 x))))))

(define (post-gremlin-name name)
  (compile/serialize/post
   *RHOST* *RPORT* name))

(let loop ()
  ;(sleep 1)
  (post-gremlin-name (make-bytes 10000))
  (loop))