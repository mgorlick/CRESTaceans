#! /usr/bin/env racket
#lang racket

(require "../../src/net/listener.rkt"
         racket/async-channel)

(define ch (make-async-channel))

(define client
  (thread
   (λ ()
     (run-listener "localhost" 5000 ch))))

(thread-send client (list 'send "localhost" 1234 #"Ping"))

(let loop ()
  (printf "~a~n" (async-channel-get ch))
  (thread-send client (list 'send "localhost" 1234 #"Ping"))
  (loop))