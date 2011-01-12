#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt"
         ffi/unsafe)

(define (sasl-plain-validation connection auth-id authorization-id password)
  (cond
    [(and (string=? auth-id "bob") (string=? password "secret")) (printf "user authenticated~n") axl-true]
    [else (printf "user not authenticated~n") axl-false]))

(define (frame-received channel connection frame user-data)
  (printf "A frame received on channel ~s~n" (vortex-channel-get-number channel))
  (if (vtx-true? (vortex-sasl-is-authenticated connection))
      (begin
        (printf "Connection is authenticated with method ~a~n" (vortex-sasl-auth-method-used connection))
        (printf "Data received: ~s~n" (vortex-frame-get-payload-string frame))
        (vortex-channel-send-rpy* channel
                                  (format "Received OK: ~s~n" (vortex-frame-get-payload-string frame))
                                  (vortex-frame-get-msgno frame))
        )
      (printf "Connection not authenticated~n"))
  (void))

(context
 [#f #f #f]
 (vortex-profiles-register context Plain-Profile-URI #f #f #f #f frame-received #f)
 (vortex-sasl-init context)
 (vortex-sasl-set-plain-validation context sasl-plain-validation)
 (let ([res (vortex-sasl-accept-negotiation context SASL-PLAIN)])
   (if (vtx-false? res)
       (printf "unable to accept SASL PLAIN profile~n")
       (printf "accepting SASL PLAIN~n")))
 (vortex-listener-new context "0.0.0.0" "44000" #f #f)
 (vortex-listener-wait context)
 )