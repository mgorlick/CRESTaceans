#! /usr/bin/env racket
#lang racket

(require "../../src/net/listener.rkt"
         racket/async-channel)

(define ch (make-async-channel))

(define server
  (thread
   (λ ()
     (run-listener "localhost" 1234 ch))))

(let loop ()
  (printf "~a~n" (async-channel-get ch))
  (thread-send server (list 'send "localhost" 5000 #"Pong"))
  (loop))