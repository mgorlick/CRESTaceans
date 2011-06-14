#! /usr/bin/env racket
#lang racket/base

(require "../src/net/listener.rkt"
         "../../old/Mischief/z.rkt"
         "../../old/Mischief/xserialize.rkt"
         "../../old/Mischief/message.rkt"
          "../../old/Mischief/tuple.rkt"
         racket/async-channel)

;; enet stuff
(define port
  (with-handlers ([exn:fail? (λ (e) 5000)])
    (string->number (vector-ref (current-command-line-arguments) 0))))
(define reply-channel (make-async-channel))
(define request-channel (run-listener "128.195.59.191" port reply-channel))

(define (compile/serialize/send host port expr)
  (define o (open-output-bytes))
  (define msg (message/ask/new #"SPAWN" #"/someurl" (mischief/compile expr) '()))
  (printf "~a~n" (serialize msg))
  (write (serialize msg) o)
  (async-channel-put request-channel (list 'send host port (get-output-bytes o))))

(define (send-gremlin)
  (define z 1)
  (compile/serialize/send
   "128.195.58.146" 1234 ; host/port
   `(lambda (t)
      (let loop ([x ,z])
        (sleep 0.5)
        (printf "Gremlin number ~a took ~a of your lug nuts~n" t x)
        (loop (add1 x))))))

(let loop ()
  (sleep 1)
  (send-gremlin)
  (loop))