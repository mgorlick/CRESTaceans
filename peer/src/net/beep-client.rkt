#lang racket

(require "../../../bindings/vortex/vortex.rkt"
         "connection-manager.rkt"
         "url.rkt"
         "../clan.rkt")

(define b->s bytes->string/utf-8)
(define s->b string->bytes/utf-8)

; a beepcli (beep client) is a struct with a vortex context,
; a map of public keys->connections and
; a reference to the clan manager
(struct beepcli (ctx conns amanager))

; set-connection-reference!: beepcli bytestring vortex-connection -> void
; store the created connection in the beep client's list of connections
; with the given REMOTE public key
(define (set-connection-reference! acli pk conn)
  (hash-set! (beepcli-conns acli) pk conn))

; make-beepcli manager -> beepcli
; create a new client which receives incoming messages from the given manager
; and validates keys from the remote peer
(define (make-beepcli amanager)
  (let ([context (new-ctx #f #f #f)])
    (vortex-sasl-init context)
    (beepcli context 
             (make-hash) 
             amanager)))

; beepcli-connect: beepcli string clan procedure any -> void
; connect the given beep client to the remote clan named by the given url
; using the credentials of the given local clan, and executing
; the given procedure on successful connectivity with the given data
(define (beepcli-connect acli clan-url aclan on-connect on-connect-data)
  (let* ([context (beepcli-ctx acli)]
         [cu (string->crest-url clan-url)]
         [host (crest-url-host cu)]
         [port (number->string (crest-url-port cu))]
         [remote-public-key-str (crest-url-public-key cu)] ; string?
         [remote-public-key-b64-bytes (s->b remote-public-key-str)] ; bytes?
         [local-public-key-b64-bytes (clan-pk-urlencoded aclan)] ; bytes? base64-url-encoded?
         [local-public-key-str (b->s local-public-key-b64-bytes)] ; string?
         ) ; string?
    
    (connection*
     [context host port on-connect on-connect-data]
     (printf "Authenticating...~n")
     ; auth-id is the local public key
     ; authorization-id is the remote public key
     ; password is the locally-computed shared key
     (vortex-sasl-set-propertie connection 'sasl-auth-id local-public-key-str #f)
     (vortex-sasl-set-propertie connection 'sasl-password remote-public-key-str #f)
     (let-values ([(status message) (vortex-sasl-start-auth-sync connection SASL-PLAIN)])
       (cond
         [(vortex-sasl-is-authenticated connection)
          (printf "authenticated with id ~a ~n" (vortex-sasl-get-propertie connection 'sasl-auth-id))
          (set-connection-reference! acli local-public-key-b64-bytes connection)
          (vortex-connection-set-data connection "crest::local-public-key" local-public-key-b64-bytes)
          (vortex-connection-set-data connection "crest::remote-public-key" remote-public-key-b64-bytes)
          #t]
         [else
          (printf "SASL negotation failed: status=~a; message=~a ~n" status message)
          (vortex-connection-close connection)
          #f])))))

(define (beepcli-disconnect acli clan-url aclan)
  (let* ([conns (beepcli-conns acli)]
         [cu (string->crest-url clan-url)]
         [remote-public-key-str (crest-url-public-key cu)])
    (cond
      [(hash-has-key? conns remote-public-key-str)
       (vortex-connection-close (hash-ref conns remote-public-key-str))
       (hash-remove! conns remote-public-key-str) 
       #t]
      [else #f])))

(provide/contract
 [make-beepcli (manager? . -> . beepcli?)]
 [beepcli? (any/c . -> . boolean?)]
 [beepcli-connect (beepcli? string? clan? (or/c procedure? #f) any/c . -> . boolean?)]
 [beepcli-disconnect (beepcli? string? clan? . -> . boolean?)])