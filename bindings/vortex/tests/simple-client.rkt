#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt")

(define (on-connect conn data) (printf "Inside VortexConnectionNew~n"))

(define (simple-client)
  (context
   ;[#f #f #f]
   [#f #t "/home/kylestrasser/racket/lib/racket/collects/openssl/test.pem"]
   (printf "connecting to localhost:44016...~n")
   (connection 
    [context "localhost" "44016" #f #f]
    (printf "opening channel....~n")
    (channel
     [connection 0 Plain-Profile-URI #f #f #f #f #f #f]
     (printf "connection to localhost:44016 ok!~n")
     (do-blocking-send-and-receive 
      wait-reply msg-no frame 
      [channel "my message"]
      (printf "the reply has arrived (size: ~s):~n     ~s~n"
              (vortex-frame-get-payload-size frame)
              (vortex-frame-get-payload-string frame)))
     ))))

(simple-client)