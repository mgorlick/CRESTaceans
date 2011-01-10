#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt")

(define (secs)
  (current-process-milliseconds))

(printf "starting client at t=~a~n" (secs))
(context
 [#f #f #f]
 (printf "context established at t=~a~n" (secs))
 (connection
  [context "localhost" "44015" #f #f]
  (printf "connection established at t=~a~n" (secs))
  (channel
   [connection 0 Plain-Profile-URI #f #f #f #f #f #f]
   (printf "channel established at t=~a~n" (secs)))
  (printf "channel closed at t=~a~n" (secs))
  )
 (printf "connection closed at t=~a~n" (secs))
 )
(printf "context cleaned at t=~a~n" (secs))
