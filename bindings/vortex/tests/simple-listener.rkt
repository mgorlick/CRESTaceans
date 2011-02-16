#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt")

(define (start-channel num connection user-data)
  (printf "Starting channel ~s~n" num)
  #t)

(define (close-channel num connection user-data)
  (printf "Closing channel ~s~n" num)
  #t)

(define (on-accepted connection data)
  (printf "New connection accepted from ~a:~a~n"
          (vortex-connection-get-host connection)
          (vortex-connection-get-port connection))
  #t)

(define (frame-received channel connection frame user-data)
  (printf "A frame received on channel ~s~n" (vortex-channel-get-number channel))
  (printf "Data received: ~s~n" (vortex-frame-get-payload-string frame))
  (vortex-channel-send-rpy* channel
                            (format "Received OK: ~s~n" (vortex-frame-get-payload-string frame))
                            (vortex-frame-get-msgno frame))
  (void))

(define (simple-listener)
  (define application-group (make-thread-group))
  (context
   ;[#f #f #f]
   [#f #t "/home/kylestrasser/racket/lib/racket/collects/openssl/test.pem"]
   (parameterize ([current-thread-group application-group])
     (printf "Server on~n")
     (vortex-profiles-register context Plain-Profile-URI start-channel #f
                               close-channel #f frame-received #f)
     (vortex-listener-new context "0.0.0.0" "44016" #f #f)
     (vortex-listener-set-on-connection-accepted context on-accepted #f)
     (printf "waiting...~n")
     (vortex-listener-wait context)
     (printf "exiting...~n")
     )))

(simple-listener)
