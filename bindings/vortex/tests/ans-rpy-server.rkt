#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt")

(define (start-channel num connection data) 
  axl-true)

(define (close-channel num connection data)
  axl-true)

(define (frame-received channel connection frame data)
  (printf "A frame received on channel ~s (message id=~s)~n" (vortex-channel-get-number channel) (vortex-frame-get-msgno frame))
  (printf "Data received: ~s~n" (vortex-frame-get-payload-string frame))
  (for/list ([i (in-range 10)])
    (let ([msg (format "Received OK (~a): ~a" i (vortex-frame-get-payload-string frame))])
      (if (vtx-false? (vortex-channel-send-ans-rpy* channel msg (vortex-frame-get-msgno frame)))
          (printf "Failed sending ANS/RPY #~s~n" i)
          (printf "Sent ANS/RPY #~s~n" i))))
  (if (vtx-false? (vortex-channel-finalize-ans-rpy channel (vortex-frame-get-msgno frame)))
      (printf "Failed sending final ANS/NUL~n")
      (printf "Sent final ANS/NUL (message id=~s)~n" (vortex-frame-get-msgno frame)))
  (void))

(context
 [#f #f #f]
 (vortex-profiles-register context Plain-Profile-URI start-channel #f close-channel #f frame-received #f)
 (vortex-listener-new context "0.0.0.0" "44000" #f #f)
 (vortex-listener-wait context))