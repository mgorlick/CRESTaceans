#lang racket

(require "../../../bindings/vortex/vortex.rkt"
         "connection-manager.rkt"
         "url.rkt")

(define s->b string->bytes/utf-8)

(define (listen ip port active? [onready #f] [onready-data #f])
  
  (define (start-channel channel-number connection user-data)
    #t)
  
  (define (close-channel channel-number connection user-data)
    #t)
  
  (define (frame-received channel connection frame user-data)
    ;; ... decrypt using the public key, 
    ;; build up a crest-message and send it 
    ;; to the manager who dispatches it to some clan ...
    (void))
  
  (define (register-pk connection auth-id authorization-id password)
    (let* ([remote-public-key-b64-bytes (s->b auth-id)] ; bytes? base64-url-encoded?
           [local-public-key-b64-bytes  (s->b password)]) ; bytes? base64-url-encoded?
      (cond
        [(active? (base64-url-decode local-public-key-b64-bytes))
         (vortex-connection-set-data connection "crest::local-public-key" local-public-key-b64-bytes)
         (vortex-connection-set-data connection "crest::remote-public-key" remote-public-key-b64-bytes)
         #t]
        [else #f]
        )))
  
  (define (child-on-close connection)
    (printf "child connection #~a on-close~n" (vortex-connection-get-id connection))
    (printf "      remote public key = ~a~n" (vortex-connection-get-data connection "crest::remote-public-key"))
    (printf "      local public key = ~a~n" (vortex-connection-get-data connection "crest::local-public-key"))
    (void))
  
  (context
   [#f #f #f]
   (vortex-profiles-register context Plain-Profile-URI start-channel #f close-channel #f frame-received #f)
   (vortex-sasl-init context)
   (vortex-sasl-set-plain-validation context register-pk)
   (let ([res (vortex-sasl-accept-negotiation context SASL-PLAIN)])
     (if res
         (printf "accepting SASL plain~n")
         (printf "unable to accept SASL plain profile~n")))
   (let ([listener (vortex-listener-new context ip port onready onready-data)])
     (vortex-connection-pass-on-close-handler listener child-on-close)
     (vortex-listener-wait context)
     (void)
     )))

(provide/contract
 [listen ([string? (or/c integer? string?) (bytes? . -> . boolean?)] 
          [procedure? any/c]
          . ->* . void)])