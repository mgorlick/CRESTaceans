#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt")

(define (client-frame-received channel connection frame user-data)
  (printf "A frame received on channel ~s~n" (vortex-channel-get-number channel))
  (printf "Message id=~s ~n" (vortex-frame-get-msgno frame))
  (printf "Data received: ~s~n" (vortex-frame-get-payload-string frame))
  (void))

(context
 (connection [context "localhost" "44000" #f #f]
  (with-vtx-channel
   channel connection 0 Plain-Profile-URI #f #f #f #f #f #f
   (vortex-channel-set-received-handler channel client-frame-received #f)
   
   (let ([msg "Hello world"])
     (vortex-channel-send-msg channel msg (string-length msg) #f)
     (printf "sent msg...~n")
     (vortex-channel-send-msg channel msg (string-length msg) #f)
     (printf "sent msg...~n"))
   (let loop () (loop))   
   (printf "Exiting client...~n")
   )))