#! /usr/bin/env racket
#lang racket/base

(require "../../src/net/listener.rkt"
         racket/match
         racket/async-channel)

(define THE-PACKET (make-bytes 0))

(define reply-received 0)
(define start-time (current-process-milliseconds #f))

(define reply-channel (make-async-channel))
(define request-channel (run-listener "localhost" 1234 reply-channel))

(let loop ()
  (match (async-channel-get reply-channel)
    [(response host port data)
     (set! reply-received (add1 reply-received))
     
     (async-channel-put request-channel (list 'send host port THE-PACKET))
     (loop)]))