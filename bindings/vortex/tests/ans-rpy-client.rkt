#! /usr/bin/env racket
#lang racket

(require ffi/unsafe
         "../vortex.rkt")

(define keep-going? #t)

(define (meta-fr)
  (let ([c 0])
    (lambda (channel connection frame user-data)
      (printf "A frame received on channel ~s~n" (vortex-channel-get-number channel))
      (printf "Message id=~s ~n" (vortex-frame-get-msgno frame))
      (printf "Data received: ~s~n" (vortex-frame-get-payload-string frame))
      (set! c (add1 c))
      (cond [(= c 11) (vortex-async-queue-push-intsignal (cast user-data _pointer _VortexAsyncQueue-pointer) 1)]))))

(define (client-frame-received channel connection frame user-data)
  (printf "A frame received on channel ~s~n" (vortex-channel-get-number channel))
  (printf "Message id=~s ~n" (vortex-frame-get-msgno frame))
  (printf "Data received: ~s~n" (vortex-frame-get-payload-string frame))
  (void))

(context
 [#f #f #f]
 (connection 
  [context "localhost" "44000" #f #f]
  (let ([q (vortex-async-queue-new)])
    (channel
     [connection 0 Plain-Profile-URI #f #f (meta-fr) q #f #f]
     (let ([msg "Hello world"])
       (vortex-channel-send-msg* channel msg #f)
       (printf "sent msg...~n"))
     (vortex-async-queue-pop q)
     (printf "Exiting client...~n")
     ))))