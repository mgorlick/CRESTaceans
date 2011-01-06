#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt")

(context
 [#f #f #f]
 (connection
  [context "localhost" "44016" #f #f]
  (printf "Established connection at t=~a~n" (current-process-milliseconds))))