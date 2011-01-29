#lang racket

(require "../../../bindings/vortex/vortex.rkt"
         "base64-url.rkt"
         "beep-message-typed.rkt")

(define s->b string->bytes/utf-8)

(define (listen ip port active? encrypter decrypter calculator validator [onready #f] [in-response #f])
  
  (define (frame-received channel connection frame user-data)
    ;; ... decrypt using the public key, 
    ;; build up a crest-message and send it 
    ;; to the manager who dispatches it to some clan ...
    (let* ([message (payload->beep-message (vortex-frame-get-payload-bytes frame))]
           [understand? (message-validate/decrypt/decode!? validator decrypter message)])
      (printf "message (valid? + decoded? ~s): ~s~n" understand? (beep-message-body message))
      (when in-response
          (in-response connection channel frame message))))
  
  (define (start-channel channel-number connection user-data) #t)
  (define (close-channel channel-number connection user-data) #t)
  
  (define (register-pk connection auth-id authorization-id password)
    (let* ([remote-public-key-b64-bytes (s->b auth-id)] ; bytes? base64-url-encoded?
           [local-public-key-b64-bytes  (s->b password)]) ; bytes? base64-url-encoded?
      (cond
        [(active? (base64-url-decode local-public-key-b64-bytes))
         (vortex-connection-set-data connection "crest::local-public-key" local-public-key-b64-bytes)
         (vortex-connection-set-data connection "crest::remote-public-key" remote-public-key-b64-bytes)
         #t]
        [else #f])))
  
  (define (child-on-close connection)
    ;; send some kind of message to clan on local side of connection through manager
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
   (let ([listener (vortex-listener-new context ip port onready #f)])
     (vortex-connection-pass-on-close-handler listener child-on-close)
     (vortex-listener-wait context)
     (void))))

(provide (all-from-out "../../../bindings/vortex/vortex.rkt"
                       "beep-message-typed.rkt"))
(provide/contract
 [listen ([string? ; host
           (or/c integer? string?) ; port
           (bytes? . -> . boolean?) ; active?
           (bytes? bytes? . -> . (values (or/c bytes? #f) (or/c bytes? #f))) ; encrypter
           (bytes? bytes? bytes? . -> . (or/c bytes? #f)) ; decrypter
           (bytes? bytes? . -> . bytes?) ; calculator
           (bytes? bytes? bytes? . -> . boolean?) ; validator 
           ] ; end mandatory arguments
          [(or/c procedure? #f)
           (or/c (VortexConnection*? VortexChannel*? VortexFrame*? beep-message? . -> . any/c) #f)] 
          ; end optional arguments
          . ->* . void)])