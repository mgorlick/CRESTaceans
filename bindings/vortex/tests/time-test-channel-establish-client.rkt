#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt")

(printf "starting client~n")
(context
 [#f #f #f]
 (connection
  [context "localhost" "44015" #f #f]
  (channel
   [connection 0 Plain-Profile-URI #f #f #f #f #f #f]
   (printf "established channel~n"))))