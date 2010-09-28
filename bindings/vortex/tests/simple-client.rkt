#! /usr/bin/env racket
#lang racket

(require ffi/unsafe
         "../vortex.rkt")

(define (on-connect conn data) (printf "Inside VortexConnectionNew~n"))

(define (simple-client)
  (with-vtx-init 
   ctx
   (printf "connecting to localhost:44000...~n")
   (with-vtx-conn 
    ctx connection "localhost" "44000" #f #f
    (with-vtx-channel
     channel connection 0 Plain-Profile-URI
     #f #f ; no close handling
     #f #f ; no frame receive async handling
     #f #f ; no async channel creation
     (let ([wait-reply (vortex-channel-create-wait-reply)]
           [msg-no (cast (malloc _int 'raw) _pointer (_ptr io _int))]) ; create a wait reply
       ; now send the message using msg_and_wait
       (let ([send-msg-and-wait-result
              (vortex-channel-send-msg-and-wait channel "my message"
                                                (string-length "my message")
                                                msg-no wait-reply)])
         (if (vtx-false? send-msg-and-wait-result)
             (begin
               (printf "unable to send my message~n")
               (vortex-channel-free-wait-reply wait-reply))
             (begin
               ; now block until the reply arrives
               ; the wait_reply object must not be freed after this function
               ; because it has already been freed
               (let ([frame (vortex-channel-wait-reply channel msg-no wait-reply)])
                 (if (null? frame)
                     (printf "there was an error while receiving the reply, or a timeout occured~n")
                     (begin
                       (printf "the reply has arrived (size: ~s):~n~s~n"
                               (vortex-frame-get-payload-size frame)
                               (cast (vortex-frame-get-payload frame) _pointer _string)))
                     ))))))))))

(simple-client)