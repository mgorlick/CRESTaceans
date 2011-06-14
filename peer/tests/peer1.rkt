#! /usr/bin/env racket
#lang racket/base

(require "../src/net/listener.rkt"
         "../../old/Mischief/z.rkt"
         "../../old/Mischief/xserialize.rkt"
         racket/async-channel)


(define (compile/serialize expr)
  (serialize (mischief/compile expr)))

(define z 1)

;; enet stuff
(define port-to-use (with-handlers ([exn:fail? (λ (e) 5000)])
                      (string->number (vector-ref (current-command-line-arguments) 0))))

(define reply-channel (make-async-channel))
(define request-channel (run-listener "localhost" port-to-use reply-channel))


(define (send-gremlin)
  (define o (open-output-bytes))
  (write (compile/serialize
          `(lambda (t)
             (let loop ([x ,z])
               (sleep 1)
               (printf "Gremlin number ~a has taken ~a of your lug nuts~n" t x)
               (loop (add1 x)))))
         o)
  (async-channel-put request-channel (list 'send "localhost" 1234 (get-output-bytes o))))

(let loop ()
  (sleep 2)
  (send-gremlin)
  (loop))