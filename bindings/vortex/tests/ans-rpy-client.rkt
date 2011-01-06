#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt")

(define keep-going? #t)

(define (meta-fr)
  (let ([c 0])
    (lambda (channel connection frame user-data)
      (printf "A frame received on channel ~s~n" (vortex-channel-get-number channel))
      (printf "Message id=~s ~n" (vortex-frame-get-msgno frame))
      (printf "Data received: ~s~n" (vortex-frame-get-payload-string frame))
      (set! c (add1 c))
      (cond [(= c 11) (set! keep-going? #f)]))))

(define (client-frame-received channel connection frame user-data)
  (printf "A frame received on channel ~s~n" (vortex-channel-get-number channel))
  (printf "Message id=~s ~n" (vortex-frame-get-msgno frame))
  (printf "Data received: ~s~n" (vortex-frame-get-payload-string frame))
  (void))

(context
 [#f #f #f]
 (connection 
  [context "localhost" "44000" #f #f]
  (channel
   [connection 0 Plain-Profile-URI #f #f #f #f #f #f]
   (vortex-channel-set-received-handler channel (meta-fr) #f)
   
   (let ([msg "Hello world"])
     (vortex-channel-send-msg* channel msg #f)
     (printf "sent msg...~n"))
   (let loop () (cond [keep-going? (loop)]))
   (printf "Exiting client...~n")
   )))