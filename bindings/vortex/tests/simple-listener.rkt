#! /usr/bin/env racket
#lang racket

(require ffi/unsafe
         "../vortex.rkt"
         "simple/simple.rkt")

(define (start-channel num connection user-data)
  (printf "In start-channel~n")
  axl-true)

(define (close-channel num connection user-data)
  (printf "In close-channel~n")
  axl-true)

(define (on-accepted connection data)
  (printf "New connection accepted from ~s:~s~n"
          (vortex-connection-get-host connection)
          (vortex-connection-get-port connection))
  axl-true)

(define (frame-received channel connection frame user-data)
  (printf "A frame received on channel: ~s~n" (vortex-channel-get-number channel))
  (printf "Data received: ~s~n" (vortex-frame-get-payload frame))
  (vortex-channel-send-rpyv channel
                            (vortex-frame-get-msgno frame)
                            (format "Received OK: ~s~n" (vortex-frame-get-payload frame)))
  (printf "VORTEX_LISTENER: end task~n")
  (void))

(with-vtx-init ctx
               (printf "Server on~n")
               ;(vortex-profiles-register ctx Plain-Profile-URI
               ;                          start-channel #f
               ;                          close-channel #f
               ;                          frame-received #f)
               (register_plain_profile ctx)
               (vortex-listener-new ctx "0.0.0.0" "44000" #f #f)
               (set_on_accepted ctx)
               ;(vortex-listener-set-on-connection-accepted ctx on-accepted #f)
               (printf "waiting...~n")
               (vortex-listener-wait ctx)
               (vortex-exit-ctx ctx axl-true)
               )