#lang racket

(require  ffi/unsafe
          "vtx/module.rkt"
          "additions/init.rkt")
(provide
 (all-defined-out)
 (all-from-out "vtx/module.rkt"))

(define (ptr-null? ptr)
  (eq? #f ptr))

; GENERAL DOCUMENTATION ON THE MACROS USED HERE
; these macros are used for initializing various objects in the
; vortex library, according to the following pattern:
; 1. create the object and bind to to a name
; 2. if the object is invalid, handle the error
; 3. if the object is valid, do something with it, then clean it up

; cleanup-and-return : lambda any ... -> ?
; save the results of the body evaluation, run the cleanup sequence,
; and then return the results of the body evaluation
(define-syntax cleanup-and-return
  (syntax-rules ()
    [(_ (body ...) (cleanup ...))
     (let ([return-value (begin body ...)])
       cleanup ... return-value)]))

(define-syntax new-ctx
  (syntax-rules ()
    [(_ use-logging? ...)
     (let ()
       (rkt:vortex-setup)
       (let ([ctx-name (vortex-ctx-new)])
         (when (not (rkt:vortex-init-ctx ctx-name use-logging? ...))
           (raise (make-exn:vtx:init "could not initialize vortex context"
                                     (current-continuation-marks))))
         ctx-name))]))

; with-vtx-ctx : identifier any ... -> ?
; initialize a vortex context and do operation(s) on that context.
; then clean up the context.
; return whatever the last value of the body evaluated to
(define-syntax with-vtx-ctx
  (syntax-rules ()
    [(_ ctx-name 
        [use-logging? ...]
        body ...)
     (let ([ctx-name (new-ctx use-logging? ...)])
       (cleanup-and-return
        (body ...)
        ((vortex-exit-ctx ctx-name #t)))
       )]))

(define-struct (exn:vtx:init exn:fail:user) ())

(define-syntax doconn
  (syntax-rules ()
    [(_ connection-name on-connected (body ...) (cleanup ...))
     ; sometimes the new connection bound to `connection-name' can be #f (null).
     ; this DOES NOT MEAN that the connection was not created;
     ; it means that you've spawned the connection in threaded
     ; (async) mode, because you supplied a callback. If this is true,
     ; the callback will need to call `vortex-connection-is-ok' inside it.
     (cond
       [(and (procedure? on-connected) (ptr-null? connection-name)) ; assume connection spawned in async mode
        (body ...)]
       [(ptr-null? connection-name) ; connection couldn't be created
        (raise (make-exn:vtx:connection 
                "unable to make connection for unknown reason" (current-continuation-marks)))]
       [(not (vortex-connection-is-ok connection-name #f)) ; connection isn't ok for some reason
        (vortex-connection-close connection-name)
        (raise (make-exn:vtx:connection
                (format "unable to connect to remote server; error was ~s" 
                        (vortex-connection-get-message connection-name))
                (current-continuation-marks)))]
       [else ; connection ok!
        (cleanup-and-return
         (body ...)
         (cleanup ...))])]))

; with-vtx-conn:
; identifier string string VortexConnectionNew any identifier any ... -> void
; supply the name of the connection, the arguments to vortex-connection-new,
; the name of the associated (valid) context, and anything to do after connecting
; then connect, execute the steps after connecting, and clean up conn once done
(define-syntax with-vtx-conn
  (syntax-rules ()
    [(_ connection-name 
        [ctx host port on-connected user-data]
        body ...)
     (let ([connection-name (vortex-connection-new ctx host port on-connected user-data)])
       (doconn connection-name on-connected (body ...) ((vortex-connection-close connection-name))))]))

; with-vtx-conn*:
; like with-vtx-conn, but do not clean up the connection automatically
(define-syntax with-vtx-conn*
  (syntax-rules ()
    [(_ connection-name 
        [ctx host port on-connected user-data]
        body ...)
     (let ([connection-name (vortex-connection-new ctx host port on-connected user-data)])
       (doconn connection-name on-connected (body ...) ('x)))]))

(define-struct (exn:vtx:connection exn:fail:network) ())

; dochann identifier
; [VortexConnection-pointer int string lambda pointer lambda pointer lambda pointer] 
; [body ...]
; [cleanup ...] -> ?
; pass the arguments to vortex-channel-new, bind the channel to the specified id,
; then do whatever is specified with the channel in scope, then do whatever cleanup requested
; channel-new can return a null reference when in threaded mode (i.e., on-created is used)

(define-syntax dochan
  (syntax-rules ()
    [(_ channel-name 
        [connection num profile on-close close-ptr on-frame-received fr-rec-ptr on-created created-ptr]
        [body ...]
        [cleanup ...])
     (let ([channel-name 
            (vortex-channel-new connection num profile
                                on-close close-ptr
                                on-frame-received fr-rec-ptr
                                on-created created-ptr
                                )])
       (if (and (procedure? on-created) (ptr-null? channel-name))
           ; assume the channel spawned in async mode
           (begin body ...)
           
           ; otherwise, test to see whether channel was created or not
           (if (ptr-null? channel-name)
               (raise (make-exn:vtx:channel "unable to create the channel" (current-continuation-marks))) ; error case
               (cleanup-and-return ; no-error case
                (body ...) 
                (cleanup ...))
               )))]))

; with-vtx-channel: initialize channel with provided name and args and
; automatically clean up the channel resources when its name leaves lexical scope
(define-syntax with-vtx-channel
  (syntax-rules ()
    [(_ channel-name 
        [channel-args ...]
        body ...)
     (dochan channel-name (channel-args ...) (body ...) ((vortex-channel-close channel-name #f)))
     ]))

; with-vtx-channel*: like with-vtx-channel, but with no automatic resource cleanup
(define-syntax with-vtx-channel*
  (syntax-rules ()
    [(_ channel-name
        [channel-args ...]
        body ...)
     (dochan channel-name (channel-args ...) (body ...) ('nothing))
     ]))

(define-struct (exn:vtx:channel exn:fail:network) ())

; do-blocking-send-and-receive: identifier identifier identifier VortexChannel-pointer string any ... -> ?
; bind the wait-reply object, the message number and the frame object to the given IDs.
; issue the given message over the given channel and wait for the other end
; to send a reply, blocking until received.
(define-syntax do-blocking-send-and-receive
  (syntax-rules ()
    [(_ wait-reply-name msgno-name frame-name 
        [channel msg]
        body ...)
     (let* ([wait-reply-name (vortex-channel-create-wait-reply)])  ; create a wait reply
       (let-values ([(send-msg-and-wait-result msgno-name) ; now actually send it
                     (vortex-channel-send-msg-and-wait* channel msg wait-reply-name)])
         (if (not send-msg-and-wait-result)
             (begin
               (vortex-channel-free-wait-reply wait-reply-name)
               (raise (make-exn:vtx:wait-and-reply "unable to send message"
                                                   (current-continuation-marks))))
             ; now block until the reply arrives. the wait_reply object 
             ; must not be freed after this function, because it has already been freed
             (let ([frame-name (vortex-channel-wait-reply channel msgno-name wait-reply-name)])
               (if (eq? #f frame-name)
                   (begin 
                     (vortex-frame-free frame-name)
                     (raise (make-exn:vtx:wait-and-reply
                             "there was an error while receiving the reply, or a timeout occured"
                             (current-continuation-marks))))
                   (begin body ...)
                   )))))]))

(define-struct (exn:vtx:wait-and-reply exn:fail:network) ())

;;; Non-hygenic convenience forms
;; the following introduce new non-hygenic bindings for the above kinds of objects
;; (contexts, connections, channels ...)
;; useful in cases where the user doesn't need to explicitly name the objects uniquely,
;; but only in general terms within a specific lexical scope

(define-syntax context
  (lambda (x)
    (syntax-case x ()
      [(k [arg1 ...] e1 e2 ...)
       (with-syntax ([context (datum->syntax #'k 'context)])
         #'(with-vtx-ctx context [arg1 ...] e1 e2 ...))])))

(define-syntax connection
  (lambda (x)
    (syntax-case x ()
      [(k [arg1 arg2 ...] e1 e2 ...)
       (with-syntax ([connection (datum->syntax #'k 'connection)])
         #'(with-vtx-conn connection [arg1 arg2 ...] e1 e2 ...))])))

(define-syntax connection*
  (lambda (x)
    (syntax-case x ()
      [(k [arg1 arg2 ...] e1 e2 ...)
       (with-syntax ([connection (datum->syntax #'k 'connection)])
         #'(with-vtx-conn* connection [arg1 arg2 ...] e1 e2 ...))])))

(define-syntax channel
  (lambda (x)
    (syntax-case x ()
      [(k [arg1 arg2 ...] e1 e2 ...)
       (with-syntax ([channel (datum->syntax #'k 'channel)])
         #'(with-vtx-channel channel [arg1 arg2 ...] e1 e2 ...))])))

(define-syntax channel*
  (lambda (x)
    (syntax-case x ()
      [(k [arg1 arg2 ...] e1 e2 ...)
       (with-syntax ([channel (datum->syntax #'k 'channel)])
         #'(with-vtx-channel* channel [arg1 arg2 ...] e1 e2 ...))])))