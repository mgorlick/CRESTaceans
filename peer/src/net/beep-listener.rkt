#lang racket

(require "../../../bindings/vortex/vortex.rkt"
         "connection-manager.rkt"
         "url.rkt")

(define s->b string->bytes/utf-8)

(define-syntax if-authed
  (syntax-rules ()
    [(_ conn (e1 ...) (e2 ...))
     (if (vtx-true? (vortex-sasl-is-authenticated conn))
         (begin e1 ...)
         (begin e2 ...))]))

(define (listen ip port amanager [onready #f] [onready-data #f])
  
  (define (start-channel channel-number connection user-data)
    ; do not start channel unless authenticated with DH
    (if-authed connection
               ((printf "starting channel on authorized connection, number ~a~n" channel-number)
                axl-true)
               ((printf "do not start channel, connection not authorized~n")
                axl-false)))
  
  (define (close-channel channel-number connection user-data)
    axl-true)
  
  (define (frame-received channel connection frame user-data)
    ;; ... build up a crest-message and send it to the manager who dispatches it to some clan ...
    (void))
  
  ; validate-connection
  ; auth-id is the request's public key
  ; authorization-id is the local clan's public key
  ; password is what the client thinks the shared key is
  (define (validate-connection connection auth-id authorization-id password)
    (let* ([conv&dec (compose base64-url-decode s->b)] ; string -> bytes? & (not base64-url-encoded?)
           [remote-public-key-decoded (conv&dec auth-id)] ; bytes? 
           [local-public-key-decoded  (conv&dec authorization-id)] ; bytes?
           [remote-shared-key-decoded (conv&dec password)]) ; bytes?
      (cond
        [(manager-key-valid? amanager 
                             local-public-key-decoded 
                             remote-public-key-decoded
                             remote-shared-key-decoded)
         (printf "Validated shared key from client claiming to be ~a~n" auth-id)
         axl-true]
        [else
         (printf "Shared key invalid from client claiming to be ~a.~n" auth-id)
         axl-false])))
  
  (context
   [#f #f #f]
   (vortex-profiles-register context Plain-Profile-URI 
                             start-channel #f
                             close-channel #f 
                             frame-received #f)
   (vortex-sasl-init context)
   (vortex-sasl-set-plain-validation context validate-connection)
   (let ([res (vortex-sasl-accept-negotiation context SASL-PLAIN)])
       (if (vtx-false? res)
           (printf "unable to accept SASL plain profile~n")
           (printf "accepting SASL plain~n")))
   (vortex-listener-new context ip port onready onready-data)
   (vortex-listener-wait context)
   (void)
   ))

(provide/contract
 [listen ([string? (or/c integer? string?) manager?] [procedure? any/c] . ->* . void)])