#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt"
         ffi/unsafe)

(context
 [#f #f #f]
 (vortex-sasl-init context)
 (connection
  [context "localhost" "44000" #f #f]
  (vortex-sasl-set-propertie connection 'sasl-auth-id "bob" #f)
  (vortex-sasl-set-propertie connection 'sasl-password "secret" #f)
  (let-values ([(status message) (vortex-sasl-start-auth-sync connection SASL-PLAIN)])
    (if (vtx-true? (vortex-sasl-is-authenticated connection))
        (begin
          (printf "authenticated with id ~s ~n" (vortex-sasl-get-propertie connection 'sasl-auth-id))
          (channel
           [connection 0 Plain-Profile-URI #f #f #f #f #f #f]
           (do-blocking-send-and-receive 
            wait-reply msg-no frame 
            [channel "my message"]
            (printf "the reply has arrived (size: ~s):~n     ~s~n"
                    (vortex-frame-get-payload-size frame)
                    (vortex-frame-get-payload-string frame)))))
        (printf "SASL negotation failed: status=~s; message=~s ~n" status message))))
 (vortex-sasl-cleanup context))