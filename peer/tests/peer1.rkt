#! /usr/bin/env racket
#lang racket/base

(require "../src/net/listener.rkt"
         "../../old/Mischief/z.rkt"
         "../../old/Mischief/xserialize.rkt"
         racket/async-channel)

;; enet stuff
(define port (with-handlers ([exn? (λ (e) 5000)]) (string->number (vector-ref (current-command-line-arguments) 0))))
(define reply-channel (make-async-channel))
(define request-channel (run-listener "127.16.121.134" port reply-channel))

(define (compile/serialize/send host port expr)
  (define o (open-output-bytes))
  (write (serialize (mischief/compile expr)) o)
  (async-channel-put request-channel (list 'send host port (get-output-bytes o))))

(define (send-gremlin)
  (define z 1)
  (compile/serialize/send "127.16.121.134" 1234
                          `(lambda (t)
                             (let loop ([x ,z])
                               (sleep 0.5)
                               (printf "Gremlin number ~a took ~a of your lug nuts~n" t x)
                               (loop (add1 x))))))

(let loop ()
  (sleep 1)
  (send-gremlin)
  (loop))