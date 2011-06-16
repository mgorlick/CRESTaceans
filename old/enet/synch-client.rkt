#! /usr/bin/env racket
#lang racket/base

(require "../../src/net/listener.rkt"
         racket/match
         racket/async-channel)

(define THE-PACKET (make-bytes 10000))

(define port-to-use (with-handlers ([exn:fail? (λ (e) 5000)])
                      (string->number(vector-ref (current-command-line-arguments) 0))))

(define reply-received 0)
(define start-time (current-process-milliseconds #f))

(define reply-channel (make-async-channel))
(define request-channel (run-listener "128.195.59.191" port-to-use reply-channel))

(let loop ()
  #|(match (async-channel-get reply-channel)
    [(response host port data)
     (set! reply-received (add1 reply-received))
     (when (equal? 0 (modulo reply-received 1000))
       (printf "received ~a replies in ~a seconds (~a replies/second)~n"
               reply-received (exact->inexact (/ (current-process-milliseconds #f) 1000))
               (exact->inexact (/ reply-received (/ (- (current-process-milliseconds #f) start-time) 1000)))))
     
     (async-channel-put request-channel (list 'send "localhost" 1234 THE-PACKET))])|#
  
  (async-channel-put request-channel (list 'send "128.195.58.146" 1234 THE-PACKET))
  (loop))