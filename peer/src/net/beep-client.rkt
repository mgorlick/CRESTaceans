#lang racket

(require "../../../bindings/vortex/vortex.rkt"
         "url.rkt"
         "../clan.rkt"
         "beep-message-support.rkt")

(define b->s bytes->string/utf-8)
(define s->b string->bytes/utf-8)

; a beepcli (beep client) is a struct with a vortex context,
; a map of public keys->connections and
; a reference to the clan manager
(struct beepcli (ctx conns chans calculator validator))
; make-beepcli
(define (make-beepcli calc valid?)
  (let ([context (new-ctx #f #f #f)])
    (vortex-sasl-init context)
    (beepcli context 
             (make-hash) ; --> ((string-append host ":" port) . connection : VortexConnection-pointer)
             (make-hash) ; --> ((cons remotepk : string . swiss-number : string
             calc valid?)))

;;; CONNECTIONS

; set-connection!: beepcli string vortex-connection -> void
; store the created connection in the beep client's list of connections
; with the given host:port
(define/contract (set-connection! acli host/port conn) [beepcli? string? VortexConnection*? . -> . void]
  (hash-set! (beepcli-conns acli) host/port conn))

; get-connection: beepcli string -> vortex-connection
; get a reference to a current connection with the host:port
(define/contract (get-connection acli host/port) [beepcli? string? . -> . (or/c VortexConnection*? #f)]
  (hash-ref (beepcli-conns acli) host/port (λ () #f)))

(define (remove-connection! acli host/port)
  (hash-remove (beepcli-conns acli) host/port))

(define (get-host/portstring curl)
  (let ([host (let ([h (crest-url-host curl)])
                (if h
                    h
                    ":nohost:"))]
        [port (let ([p (crest-url-port curl)])
                (if p
                    (number->string p)
                    ":noport:"))])
    (string-append host ":" port)))

; beepcli-connect: beepcli string clan procedure any -> void
; connect the given beep client to the host named by the given url
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
    (printf "~a: Conversions finished~n" (current-process-milliseconds))
    (with-handlers ([exn:vtx:connection? (λ (e) #f)])
      (connection*
       [context host port on-connect on-connect-data]
       (printf "~a: Connection negotiated~n" (current-process-milliseconds))
       (vortex-connection-set-data connection "crest::local-public-key" local-public-key-b64-bytes)
       (vortex-connection-set-data connection "crest::remote-public-key" remote-public-key-b64-bytes)
       (set-connection! acli (get-host/portstring cu) connection)
       #t))))

(define (beep/disconnect acli url aclan)
  (let* ([cu (string->crest-url url)]
         [hps (get-host/portstring cu)]
         [conn (get-connection acli hps)]
         [remote-public-key-str (crest-url-public-key cu)])
    (if conn
        (begin 
          (vortex-connection-close conn)
          (remove-connection! acli remote-public-key-str hps)
          #t)
        #f)))

;;; CHANNELS

(define/contract (set-channel! acli hostport pk chan) [beepcli? string? string? VortexChannel*? . -> . void]
  (hash-set! (beepcli-chans acli) (cons hostport pk) chan))

(define/contract (get-channel acli hostport pk) [beepcli? string? string? . -> . (or/c #f VortexChannel*?)]
  (hash-ref (beepcli-chans acli) (cons hostport pk) (λ () #f)))

(define/contract (remove-channel! acli pk hostport) [beepcli? string? string? . -> . void]
  (hash-remove (beepcli-chans acli) (cons hostport pk)))

; beep/start-channel: beepcli stringclan -> void
; establish a channel to the remote clan identified by the (pk . host/port)
(define (beep/start-channel acli url aclan [on-received #f])
  
  (define (frame-received channel connection frame user-data)    
    (let-values ([(message understand?) (frame->message frame (beepcli-validator acli))])
      (printf "message (valid? + decoded? ~s): ~s~n" understand? (beep-message-body message))
      (when on-received (on-received connection channel frame message)))
    (void))
  
  (let* ([context (beepcli-ctx acli)]
         [cu (string->crest-url url)]
         [connection (get-connection acli (get-host/portstring cu))]
         [remote-public-key (crest-url-public-key cu)]
         [swiss-number (crest-url-swiss-num cu)])
    (with-handlers ([exn:vtx:channel? (λ (e) #f)])
      (channel*
       [connection 0 Plain-Profile-URI #f #f frame-received #f #f #f]
       (set-channel! acli remote-public-key (get-host/portstring cu) channel)
       #t
       ))))

(define (beep/close-channel acli url aclan)
  (let* ([cu (string->crest-url url)]
         [remote-public-key (crest-url-public-key cu)]
         [swiss-number (crest-url-swiss-num cu)]
         [channel (get-channel acli remote-public-key swiss-number)])
    (remove-channel! acli remote-public-key (get-host/portstring cu))
    (vortex-channel-close channel #f)
    #t))

;;; MESSAGES

;; beep/msg: send a string message to the clan member identified
; by the (remote-public-key . host/port) pair
(define (beep/msg acli url aclan msg)
  
  (let* ([cu (string->crest-url url)]
         [remote-public-key (crest-url-public-key cu)]
         [remote-public-key-encoded (string->bytes/utf-8 remote-public-key)]
         [remote-public-key-bytes (base64-url-decode remote-public-key-encoded)]
         [swiss-number (crest-url-swiss-num cu)]
         [channel (get-channel acli remote-public-key (get-host/portstring cu))]
         [local-public-key-encoded (clan-pk-urlencoded aclan)])
    (if channel
        (let* ([msg-bytes-encoded (message-encode msg)]
               [mac-encoded (mac-calculate/encode 
                             (beepcli-calculator acli) msg-bytes-encoded remote-public-key-bytes)]
               [message (make-beep-message 
                         local-public-key-encoded
                         remote-public-key-encoded 
                         mac-encoded
                         msg-bytes-encoded)]
               [payload (beep-message->payload message)])
          (let-values ([(success? msgno) (vortex-channel-send-msg channel payload)])
            success?))
        #f)))

(provide/contract
 [make-beepcli ((bytes? bytes? . -> . bytes?) ; MAC calculator
                (bytes? bytes? bytes? . -> . boolean?) ; MAC validator
                . -> . beepcli?)]
 [beepcli? (any/c . -> . boolean?)]
 [beep/connect ([beepcli? string? clan?] [(or/c procedure? #f) any/c] . ->* . boolean?)]
 [beep/disconnect (beepcli? string? clan? . -> . boolean?)]
 [beep/start-channel ([beepcli? string? clan?]
                      [(VortexConnection*? VortexChannel*? VortexFrame*? beep-message? . -> . any/c)]
                      . ->* . boolean?)]
 [beep/close-channel (beepcli? string? clan? . -> . boolean?)]
 [beep/msg (beepcli? string? clan? string? . -> . boolean?)])