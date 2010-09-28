#lang racket

(require "vtx/channel-pool.rkt"
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
 (all-defined-out)
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

; with-vtx-init : identifier any ... -> ?
; initialize a vortex context and do operation(s) on that context.
; then clean up the context but don't exit it.
; return whatever the last value of the body evaluated to
(define-syntax with-vtx-init
  (syntax-rules ()
    [(_ ctx-name 
        body ...)
     (let ([ctx-name (vortex-ctx-new)])
       (if (vtx-false? (vortex-init-ctx ctx-name))
           (raise (make-exn:vtx:init "could not initialize vortex context~n"
                                     (current-continuation-marks)))
           (begin
             (let ([return-val
                    (begin
                      body
                      ...)])
               (vortex-exit-ctx ctx-name axl-false)
               (vortex-ctx-free ctx-name)
               return-val))))]
    ))

(define-struct (exn:vtx:init exn:fail:user) ())

; with-vtx-connection:
; identifier string string VortexConnectionNew any identifier any ... -> void
; supply the name of the connection, the arguments to vortex-connection-new,
; the name of the associated (valid) context, and anything to do after connecting
; then connect, execute the steps after connecting, and exit vortex once completed
(define-syntax with-vtx-conn
  (syntax-rules ()
    [(_ ctx connection-name 
        host port on-connected user-data 
        body ...)
     (let ([connection-name (vortex-connection-new ctx host port on-connected user-data)])
       (if (vtx-false? (vortex-connection-is-ok connection-name axl-false))
           ; sometimes the new connection bound to `connection-name' can be #f (null).
           ; this DOES NOT MEAN that the connection was not created;
           ; it could mean that you've spawned the connection in threaded
           ; (async) mode, because you supplied a callback.
           ; always check with vortex-connection-is-ok, rather than
           ; checking for a null value.
           
           (begin
             (vortex-connection-close connection-name)
             (raise (make-exn:vtx:connection
                     (format "unable to connect to remote server; error was ~s" 
                             (vortex-connection-get-message connection-name))
                     (current-continuation-marks))
                    ))
           (let ([return-val
                  (begin
                    (printf "connection to ~a:~a ok!~n" host port)
                    body
                    ...)])
             (cond [(not (eq? #f connection-name)) (vortex-connection-close connection-name)])
             return-val)
           ))]
    ))

(define-struct (exn:vtx:connection exn:fail:network) ())

; with-vtx-channel
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
           (let ([return-val 
                  (begin
                    body
                    ...)])
             (vortex-channel-close channel-name #f)
             return-val)))]))

(define-struct (exn:vtx:channel exn:fail:network) ())