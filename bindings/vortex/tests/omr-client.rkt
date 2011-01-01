#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt")

(define done-sending? #f)
(define exit? #f)

(define (client-frame-received channel connection frame user-data)
  (printf "A frame received on channel ~s~n" (vortex-channel-get-number channel))
  (printf "Message id=~s ~n" (vortex-frame-get-msgno frame))
  (printf "Data received: ~s~n" (vortex-frame-get-payload-string frame))
  (cond [done-sending? (set! exit? #t)])
  (void))

(with-vtx-ctx
 ctx
 (printf "connecting to localhost:44000...~n")
 (with-vtx-conn 
  connection ctx "localhost" "44000" #f #f
  (printf "opening channel....~n")
  (with-vtx-channel
   channel connection 0 Plain-Profile-URI
   #f #f ; no close handling
   #f #f ; frame receive async handling
   #f #f ; no async channel creation
   (vortex-channel-set-received-handler channel client-frame-received #f)
   (printf "connection to localhost:44000 ok!~n")
   
   (let ([msg "Hello world"])
     (vortex-channel-send-msg channel msg (string-length msg) #f)
     (printf "sent msg...~n")
     (vortex-channel-send-msg channel msg (string-length msg) #f)
     (printf "sent msg...~n"))
   
   (set! done-sending? #t)
   (let loop ()
     (cond [(not exit?) (loop)]))
   
   (printf "Exiting client...~n")
   )))