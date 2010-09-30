#lang racket

(require 
 ffi/unsafe
 "vtx/channel-pool.rkt"
 "vtx/channel.rkt"
 "vtx/connection.rkt"
 "vtx/connection-options.rkt"
 "vtx/context.rkt"
 "vtx/frame-factory.rkt"
 "vtx/greetings.rkt"
 "vtx/hash.rkt"
 "vtx/io.rkt"
 "vtx/libvortex.rkt"
 "vtx/listeners.rkt"
 "vtx/main-init-and-exit.rkt"
 "vtx/profiles.rkt"
 "vtx/queue.rkt"
 "vtx/reader.rkt"
 "vtx/support.rkt"
 "vtx/thread-pool.rkt"
 "vtx/threads.rkt"
 "vtx/addons/alive.rkt"
 "vtx/addons/pull.rkt"
 "vtx/addons/sasl.rkt"
 "vtx/addons/tls.rkt"
 "vtx/addons/tunnel.rkt")
(provide
 (all-from-out "vtx/channel-pool.rkt"
               "vtx/channel.rkt"
               "vtx/connection.rkt"
               "vtx/connection-options.rkt"
               "vtx/context.rkt"
               "vtx/frame-factory.rkt"
               "vtx/greetings.rkt"
               "vtx/hash.rkt"
               "vtx/io.rkt"
               "vtx/libvortex.rkt"
               "vtx/listeners.rkt"
               "vtx/main-init-and-exit.rkt"
               "vtx/profiles.rkt"
               "vtx/queue.rkt"
               "vtx/reader.rkt"
               "vtx/support.rkt"
               "vtx/thread-pool.rkt"
               "vtx/threads.rkt"
               "vtx/addons/alive.rkt"
               "vtx/addons/pull.rkt"
               "vtx/addons/sasl.rkt"
               "vtx/addons/tls.rkt"
               "vtx/addons/tunnel.rkt"))

; GENERAL DOCUMENTATION ON THE MACROS USED HERE
; these macros are used for initializing various objects in the
; vortex library, according to the following pattern:
; 1. create the object and bind to to a name
; 2. if the object is invalid, handle the error
; 3. if the object is valid, do something with it, then clean it up

; the following macro skeleton is provided for clarity.
; A. (i) macro args used to bind a name to the new object, and also cleanup etc
; A. (ii) and (iii) arguments to the actual object `factory-like' function
; A. (iv) whatever you want to do with the object once it's created,
;        using the name defined in (i)
; B. create the object
; C. check validity
; D. if valid, bind a temp return value to whatever the result of the body is
; E. clean up the created object somehow
; F. return the return value
; G. if invalid handle the error
;(define-syntax macro-name
;  (syntax-rules ()
;    [(_ arg1 arg2 arg3 ; A(i) 
;        arg4 arg5 arg6 ; A(ii)
;        arg7 arg8 arg9 ; A(iii)
;        body ...) ; A(iv)
;     (let ([arg1 (call-to-factory-func arg4 arg5 arg6 arg7 arg8 arg9)]) ; B
;       (if (is-valid? arg1) ; C
;           (begin
;             (let ([return-val (begin body ...)]) ; D
;               (cleanup-after-body) ; E
;               return-val)) ; F
;           (handle-error) ; G
;           ))]))

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
; then clean up the context but don't exit it.
; return whatever the last value of the body evaluated to
(define-syntax with-vtx-ctx
  (syntax-rules ()
    [(_ ctx-name 
        body ...)
     (let ([ctx-name (vortex-ctx-new)])
       (if (vtx-false? (vortex-init-ctx ctx-name))
           (raise (make-exn:vtx:init "could not initialize vortex context~n"
                                     (current-continuation-marks)))
           (cleanup-and-return
            (body ...) ((vortex-exit-ctx ctx-name axl-false) (vortex-ctx-free ctx-name)))
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
       (if (vtx-false? (vortex-connection-is-ok connection-name axl-false))
           ; sometimes the new connection bound to `connection-name' can be #f (null).
           ; this DOES NOT MEAN that the connection was not created;
           ; it means that you've spawned the connection in threaded
           ; (async) mode, because you supplied a callback. If this is true,
           ; the callback will need to call `vortex-connection-is-ok' inside it.
           ; here we only deal with the case where the connection is NOT threaded
           ; (i.e., a null value for the `on-connected' callback was provided.)
           (begin
             (vortex-connection-close connection-name)
             (raise (make-exn:vtx:connection
                     (format "unable to connect to remote server; error was ~s" 
                             (vortex-connection-get-message connection-name))
                     (current-continuation-marks))
                    ))
           (cleanup-and-return
            (body ...)
            ((cond [(not (eq? #f connection-name)) (vortex-connection-close connection-name)]))
            )))]
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
       (if (null? channel-name)
           (raise (make-exn:vtx:channel "unable to create the channel" 
                                        (current-continuation-marks)))
           (cleanup-and-return (body ...) ((vortex-channel-close channel-name #f)))
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