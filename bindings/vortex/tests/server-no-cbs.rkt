#! /usr/bin/env racket
#lang racket

(require 
 (except-in ffi/unsafe ->)
 racket/contract 
 "../vortex.rkt"
 "no-cbs/no-cbs.rkt")

(define Profile-URI "http://vortex.aspl.es/profiles/example")

(define/contract (queue-reply channel connection frame user-data)
  (VortexChannel*? VortexConnection*? VortexFrame*? cpointer? . -> . void?)
  (let ([queue (cast user-data _pointer _VortexAsyncQueue-pointer)]
        [frame-copy (vortex-frame-copy frame)])
    (vortex-async-queue-push queue frame-copy)
    ))

(with-vtx-ctx
 ctx
 (let ([queue (vortex-async-queue-new)])
   (register_nocbs_profile ctx queue) ; call to special wrapper due to callbacks being broken
   ; (the callbacks are not even asynchronous in this case; something fundamental is broken)
   (printf "New listener created: ~s~n" (vortex-listener-new ctx "0.0.0.0" "44000" #f #f))
   (let loop ([iterator 0])
     (printf "inside (loop ~s)~n" iterator)
     (let ([frame (cast (vortex-async-queue-pop queue) _pointer _VortexFrame-pointer)])
       (if (null? frame)
           (loop)
           #f)
       (printf "Frame recieved, content: ~s~n" (cast (vortex-frame-get-payload frame) _pointer _string))
       (let ([channel (vortex-frame-get-channel-ref frame)])
         (vortex-channel-send-rpy channel (cast (vortex-frame-get-payload frame) _pointer _string)
                                  (vortex-frame-get-payload-size frame) (vortex-frame-get-msgno frame))
         (vortex-frame-free frame)
         (if (= (+ iterator 1) 10)
             (begin (printf "here I'm closing the channel~n")
                    (vortex-channel-close channel #f)
                    (loop 0))
             (loop (+ iterator 1))
             )
         ))
     )
   )
 )