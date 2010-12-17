#lang racket

(require  ffi/unsafe
          "vtx/module.rkt"
          "additions/init.rkt")
(provide
 (all-from-out "vtx/module.rkt"
               "additions/init.rkt"))

; GENERAL DOCUMENTATION ON THE MACROS USED HERE
; these macros are used for initializing various objects in the
; vortex library, according to the following pattern:
; 1. create the object and bind to to a name
; 2. if the object is invalid, handle the error
; 3. if the object is valid, do something with it, then clean it up

; (in principle we should be able to write a macro-generating-macro
; to generate all of these at once, but for now it's easiest to just 
; write them by hand for exploratory design purposes, as they may diverge
; in ways that would make it harder to keep a macro-generating-macro generic enough)

; cleanup-and-return : lambda any ... -> ?
; save the results of the body evaluation, run the cleanup sequence,
; and then return the results of the body evaluation
(define-syntax cleanup-and-return
  (syntax-rules ()
    [(_ (body ...) (cleanup ...))
     (let ([return-value (begin body ...)])
       cleanup ... return-value)]))

; with-vtx-ctx : identifier any ... -> ?
; initialize a vortex context and do operation(s) on that context.
; then clean up the context.
; return whatever the last value of the body evaluated to
(define-syntax with-vtx-ctx
  (syntax-rules ()
    [(_ ctx-name 
        body ...)
     (let ([ctx-name (vortex-ctx-new)])
       (if (vtx-false? (rkt:vortex-init-ctx ctx-name))
           (raise (make-exn:vtx:init "could not initialize vortex context"
                                     (current-continuation-marks)))
           (cleanup-and-return
            (body ...)
            ((printf "cleaning up ctx~n")
             (vortex-exit-ctx ctx-name axl-false) 
             (vortex-ctx-free ctx-name)))
           ))]
    ))

(define-struct (exn:vtx:init exn:fail:user) ())

; with-vtx-connection:
; identifier string string VortexConnectionNew any identifier any ... -> void
; supply the name of the connection, the arguments to vortex-connection-new,
; the name of the associated (valid) context, and anything to do after connecting
; then connect, execute the steps after connecting, and exit vortex once completed
(define-syntax with-vtx-conn
  (syntax-rules ()
    [(_ connection-name 
        ctx host port on-connected user-data 
        body ...)
     (let ([connection-name (vortex-connection-new ctx host port on-connected user-data)])
       (printf "made new connection object~n")
       ; sometimes the new connection bound to `connection-name' can be #f (null).
       ; this DOES NOT MEAN that the connection was not created;
       ; it means that you've spawned the connection in threaded
       ; (async) mode, because you supplied a callback. If this is true,
       ; the callback will need to call `vortex-connection-is-ok' inside it.
       ; here we only deal with the case where the connection is NOT threaded
       ; (i.e., a null value for the `on-connected' callback was provided.)
       (if (eq? #f connection-name)
           ; assume that the connection spawned in async mode
           (cleanup-and-return
            (body ...)
            ((cond [(not (eq? #f connection-name)) 
                    (vortex-connection-close connection-name)])))
           
           (if (vtx-false? (vortex-connection-is-ok connection-name axl-false))
               (begin
                 (vortex-connection-close connection-name)
                 (raise (make-exn:vtx:connection
                         (format "unable to connect to remote server; error was ~s" 
                                 (vortex-connection-get-message connection-name))
                         (current-continuation-marks))
                        ))
               (cleanup-and-return
                (body ...)
                ((cond [(not (eq? #f connection-name)) 
                        (printf "closing connection~n")
                        (vortex-connection-close connection-name)]))
                ))))]
    ))

(define-struct (exn:vtx:connection exn:fail:network) ())

; with-vtx-channel identifier VortexConnection-pointer int string
;                  lambda pointer lambda pointer lambda pointer body ... -> ?
; pass the arguments to vortex-channel-new, bind the channel to the specified id,
; then do whatever is specified with the channel in scope, then close the channel
(define-syntax with-vtx-channel
  (syntax-rules ()
    [(_ channel-name 
        connection num profile
        on-close close-ptr 
        on-frame-received fr-rec-ptr
        on-created created-ptr
        body ...)
     (let ([channel-name 
            (vortex-channel-new connection num profile
                                on-close close-ptr
                                on-frame-received fr-rec-ptr
                                on-created created-ptr
                                )])
       (if (eq? #f channel-name)
           (raise (make-exn:vtx:channel "unable to create the channel" 
                                        (current-continuation-marks)))
           (cleanup-and-return (body ...) ((printf "closing channel ~s~n" channel-name) (vortex-channel-close channel-name #f)))
           ))]))

(define-struct (exn:vtx:channel exn:fail:network) ())

; do-blocking-send-and-receive: identifier identifier identifier VortexChannel-pointer string any ... -> ?
; bind the wait-reply object, the message number and the frame object to the given IDs.
; issue the given message over the given channel and wait for the other end
; to send a reply, blocking until received.
(define-syntax do-blocking-send-and-receive
  (syntax-rules ()
    [(_ wait-reply-name msgno-name frame-name 
        channel msg
        body ...)
     (let* ([wait-reply-name (vortex-channel-create-wait-reply)]  ; create a wait reply
            [msgno-name (cast (malloc _int) _pointer (_ptr io _int))]
            [send-msg-and-wait-result ; now actually send it
             (vortex-channel-send-msg-and-wait channel msg
                                               (string-length msg)
                                               msgno-name wait-reply-name)])
       (if (vtx-false? send-msg-and-wait-result)
           (begin
             (vortex-channel-free-wait-reply wait-reply-name)
             (raise (make-exn:vtx:wait-and-reply "unable to send message"
                                                 (current-continuation-marks))))
           ; now block until the reply arrives. the wait_reply object 
           ; must not be freed after this function, because it has already been freed
           (let ([frame-name (vortex-channel-wait-reply channel msgno-name wait-reply-name)])
             (if (null? frame-name)
                 (begin 
                   (vortex-frame-free frame-name)
                   (raise (make-exn:vtx:wait-and-reply
                           "there was an error while receiving the reply, or a timeout occured"
                           (current-continuation-marks))))
                 (cleanup-and-return (body ...) ())
                 ))))]
    ))

(define-struct (exn:vtx:wait-and-reply exn:fail:network) ())

(provide (all-defined-out))