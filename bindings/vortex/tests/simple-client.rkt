#! /usr/bin/env racket
#lang racket

(require ffi/unsafe
         "../vortex.rkt")

(define (on-connect conn data) (printf "Inside VortexConnectionNew~n"))

(define (simple-client)
  (with-vtx-ctx
   ctx
   (printf "connecting to localhost:44000...~n")
   (with-vtx-conn 
    connection ctx "localhost" "44000" #f #f
    (printf "opening channel....~n")
    (with-vtx-channel
     channel connection 0 Plain-Profile-URI
     #f #f ; no close handling
     #f #f ; no frame receive async handling
     #f #f ; no async channel creation
     (printf "connection to localhost:44000 ok!~n")
     (do-blocking-send-and-receive wait-reply msg-no frame channel "my message"
      (printf "the reply has arrived (size: ~s):~n     ~s~n"
              (vortex-frame-get-payload-size frame)
              (cast (vortex-frame-get-payload frame) _pointer _string)))
     ))))

(simple-client)