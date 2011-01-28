#lang racket

(require "../../../bindings/vortex/vortex.rkt"
         "url.rkt"
         "../clan.rkt"
         "beep-message.rkt")

(define b->s bytes->string/utf-8)
(define s->b string->bytes/utf-8)

; a beepcli (beep client) is a struct with a vortex context,
; a map of public keys->connections and
; a reference to the clan manager
(struct beepcli (ctx conns chans encrypter decrypter calculator validator))

; make-beepcli
(define (make-beepcli encrypt decrypt calc valid?)
  (let ([context (new-ctx #f #f #f)])
    (vortex-sasl-init context)
    (beepcli context 
             (make-hash) ; --> (remotepk : bytestring . connection : VortexConnection-pointer)
             (make-hash) ; --> ((cons remotepk : string . swiss-number : string
             encrypt decrypt calc valid?)))

;;; CONNECTIONS

; set-connection!: beepcli string vortex-connection -> void
; store the created connection in the beep client's list of connections
; with the given base64-url-encoded remote public key
(define/contract (set-connection! acli pk conn) [beepcli? string? VortexConnection*? . -> . void]
  (hash-set! (beepcli-conns acli) pk conn))

; get-connection: beepcli string -> vortex-connection
; get a reference to a current connection with the corresponding
; base64-url-encoded remote public key
(define/contract (get-connection acli pk) [beepcli? string? . -> . (or/c VortexConnection*? #f)]
  (hash-ref (beepcli-conns acli) pk (λ () #f)))

; beepcli-connect: beepcli string clan procedure any -> void
; connect the given beep client to the remote clan named by the given url
; using the credentials of the given local clan, and executing
; the given procedure on successful connectivity with the given data
(define (beep/connect acli url aclan [on-connect #f] [on-connect-data #f])
  (let* ([context (beepcli-ctx acli)]
         [cu (string->crest-url url)]
         [host (crest-url-host cu)]
         [port (number->string (crest-url-port cu))]
         [remote-public-key-str (crest-url-public-key cu)] ; string?
         [remote-public-key-b64-bytes (s->b remote-public-key-str)] ; bytes?
         [local-public-key-b64-bytes (clan-pk-urlencoded aclan)] ; bytes? base64-url-encoded?
         [local-public-key-str (b->s local-public-key-b64-bytes)] ; string?
         ) ; string?
    
    (connection*
     [context host port on-connect on-connect-data]
     ; auth-id is the local public key
     ; authorization-id is the remote public key
     ; password is the locally-computed shared key
     (vortex-sasl-set-propertie connection 'sasl-auth-id local-public-key-str #f)
     (vortex-sasl-set-propertie connection 'sasl-password remote-public-key-str #f)
     (let-values ([(status message) (vortex-sasl-start-auth-sync connection SASL-PLAIN)])
       (printf "~a~n" message)
       (cond
         [(vortex-sasl-is-authenticated connection)
          (set-connection! acli remote-public-key-str connection)
          (vortex-connection-set-data connection "crest::local-public-key" local-public-key-b64-bytes)
          (vortex-connection-set-data connection "crest::remote-public-key" remote-public-key-b64-bytes)
          #t]
         [else
          (vortex-connection-close connection)
          #f])))))

(define (beep/disconnect acli url aclan)
  (let* ([conns (beepcli-conns acli)]
         [cu (string->crest-url url)]
         [remote-public-key-str (crest-url-public-key cu)])
    (cond
      [(hash-has-key? conns remote-public-key-str)
       (vortex-connection-close (hash-ref conns remote-public-key-str))
       (hash-remove! conns remote-public-key-str) 
       #t]
      [else #f])))

;;; CHANNELS

(define/contract (set-channel! acli pk swissnum chan) [beepcli? string? string? VortexChannel*? . -> . void]
  (hash-set! (beepcli-chans acli) (cons pk swissnum) chan))

(define/contract (get-channel acli pk swissnum) [beepcli? string? string? . -> . VortexChannel*?]
  (hash-ref (beepcli-chans acli) (cons pk swissnum) (λ () #f)))

; beep/start-channel: beepcli stringclan -> void
; establish a connection to the remote clan member identified by the swiss number
; in the given url
(define (beep/start-channel acli url aclan)
  (define (frame-received channel connection frame user-data)    
    (let* ([message (payload->beep-message (vortex-frame-get-payload-bytes frame))]
           [understand? (message-validate/decrypt/decode!? (beepcli-validator acli) (beepcli-decrypter acli) message)])
      (printf "message (valid? + decoded? ~s): ~s~n" understand? (beep-message-body message))))
  
  (let* ([context (beepcli-ctx acli)]
         [cu (string->crest-url url)]
         [remote-public-key (crest-url-public-key cu)]
         [swiss-number (crest-url-swiss-num cu)]
         [connection (get-connection acli remote-public-key)])
    (channel*
     [connection 0 Plain-Profile-URI #f #f frame-received #f #f #f]
     (set-channel! acli remote-public-key swiss-number channel)
     #t
     )))

(define (beep/close-channel acli url aclan)
  ;; ... close channel here ...
  #f)

;;; MESSAGES

;; beep/msg: send a string message to the computation identified
; by the (remote-public-key . swiss-number) pair
(define (beep/msg acli url aclan msg)
  
  (let* ([msg-bytes (string->bytes/utf-8 msg)]
         [cu (string->crest-url url)]
         [remote-public-key (crest-url-public-key cu)]
         [remote-public-key-encoded (string->bytes/utf-8 remote-public-key)]
         [remote-public-key-bytes (base64-url-decode remote-public-key-encoded)]
         [swiss-number (crest-url-swiss-num cu)]
         [channel (get-channel acli remote-public-key swiss-number)]
         [local-public-key-encoded (clan-pk-urlencoded aclan)])
    ; first, encrypt and encode the message, 
    ; get back the encoded versions of the
    ; message and initialization vector
    (let-values ([(msg-bytes-encoded iv-encoded) 
                  (message-encrypt/encode (beepcli-encrypter acli)
                                          msg-bytes
                                          remote-public-key-bytes)])
      ; assemble payload
      (let* ([mac-encoded (mac-calculate/encode 
                           (beepcli-calculator acli) msg-bytes-encoded remote-public-key-encoded)]
             [message (beep-message local-public-key-encoded
                                    remote-public-key-encoded 
                                    iv-encoded 
                                    mac-encoded
                                    msg-bytes-encoded)]
             [payload (beep-message->payload message)])
        (let-values ([(success? msgno) (vortex-channel-send-msg channel payload)])
          success?)))))

(provide/contract
 [make-beepcli ((bytes? bytes? . -> . (values bytes? bytes?)) ; encrypter
                (bytes? bytes? bytes? . -> . bytes?) ; decrypter
                (bytes? bytes? . -> . bytes?) ; MAC calculator
                (bytes? bytes? bytes? . -> . boolean?) ; MAC validator
                . -> . beepcli?)]
 [beepcli? (any/c . -> . boolean?)]
 [beep/connect ([beepcli? string? clan?] [(or/c procedure? #f) any/c] . ->* . boolean?)]
 [beep/disconnect (beepcli? string? clan? . -> . boolean?)]
 [beep/start-channel (beepcli? string? clan? . -> . boolean?)]
 [beep/close-channel (beepcli? string? clan? . -> . boolean?)]
 [beep/msg (beepcli? string? clan? string? . -> . boolean?)])