#! /usr/bin/env racket
#lang racket

(require ffi/unsafe
         "../vortex.rkt")

(define (frame-received)
  (let ([c 0])
    (lambda (channel connection frame user-data)
      (printf "A frame received on channel ~s~n" (vortex-channel-get-number channel))
      (printf "Message id=~s ~n" (vortex-frame-get-msgno frame))
      (printf "Data received: ~s~n" (vortex-frame-get-payload-string frame))
      (set! c (add1 c))
      (cond [(= c 11) (vortex-async-queue-push-intsignal (cast user-data _pointer _VortexAsyncQueue-pointer) 1)]))))

(context
 [#f #f #f]
 (connection 
  [context "localhost" "44000" #f #f]
  (let ([q (vortex-async-queue-new)])
    (channel
     [connection 0 Plain-Profile-URI #f #f (frame-received) q #f #f]
     (let ([msg "Hello world"])
       (let-values ([(x y) (vortex-channel-send-msg* channel msg)])
       (printf "sent msg...(~a, ~a)~n" x y)))
     (vortex-async-queue-pop q)
     (printf "Exiting client...~n")
     ))))