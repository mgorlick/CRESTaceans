#lang racket/base

(require "../../../bindings/enet/enet.rkt"
         racket/contract
         racket/match
         racket/function
         racket/async-channel)

(provide (all-defined-out))

(struct response (host port data))

(define port/c (and/c exact-integer? (between/c 0 65535)))
(define peersHT/c (hash/c (cons/c exact-nonnegative-integer? port/c) cpointer?))
(define suspmessageHT/c (hash/c (cons/c exact-nonnegative-integer? port/c) (listof cpointer?)))

(define/contract (run-listener host port reply-channel)
  (string? port/c async-channel? . -> . async-channel?)
  
  (define enet-host-listening-addr (host/port->addr host port))
  (define enet-host (enet-host-create enet-host-listening-addr ENetMaxPeerCount 0 0 0))
  
  ;; map (ip . port) cons cells to connected ENetPeer *ptrs
  (define/contract enet-peers peersHT/c (make-hash))
  
  ;; map delayed (ip . port) cons cells to lists of ENetPacket *ptrs intended
  ;; for the corresponding ENetPeer in the enet-peers table, once that peer connects
  ;; we have to handle messages submitted pre-connect this way, because
  ;; enet will drop the message silently if we call enet-host-connect and
  ;; call enet-peer-send immediately afterward
  (define/contract susp-messages suspmessageHT/c (make-hash))
  
  (define request-channel (make-async-channel))
  
  (define/contract (send-message hostname port data)
    (string? port/c bytes? . -> . void)
    (let ([peer (lookup-peer enet-peers hostname port)])
      (if peer
          (enet-peer-send peer 0 (enet-packet-create/bytes data 'reliable))
          ; peer not connected yet. queue up the message in the racket-level queue
          ; (don't submit it to enet yet since enet will silently throw it away before the peer finishes connection)
          (begin (enet-host-connect-by-name enet-host hostname port 1 0)
                 (suspend-message susp-messages hostname port
                                  (enet-packet-create/bytes data '(reliable)))))))
  
  (define event-loop
    (thread
     (λ ()
       (let loop ()
         
         ;; first look for an outgoing message
         (match (sync/timeout 0 request-channel)
           [(list 'send (? string? host) (? exact-nonnegative-integer? port) (? bytes? data))
            (send-message host port data)]
           [#f #f]
           [any (printf "Listener dropping outgoing message: ~a~n" any)])
         
         ;; then look for an incoming event
         (let ([event (enet-host-service enet-host 0)])
           (when event
             (match (ENetEvent-type event)
               ['connect
                (define peer (ENetEvent-peer event))
                ;; clear the peer's entry in suspended messages table, if present
                (define msgs (get-susp-messages/clear-entry! susp-messages (ENetPeer-address peer)))
                ;; move the completed peer to the connected peer table
                (register-peer enet-peers peer)
                (foldr (λ (pkt v) (enet-peer-send peer 0 pkt) v) (void) msgs) ;; foldr preserves ordering
                (printf "Peer connected~n")]
               
               ['disconnect
                (unregister-peer enet-peers (ENetEvent-peer event))
                (printf "Peer disconnected~n")]
               
               ['receive
                (async-channel-put
                 reply-channel
                 (response "localhost"
                           (event-port event)
                           (preprocess-message (copy-packet-data (ENetEvent-packet event)))))
                (enet-packet-destroy (ENetEvent-packet event))])))
         (loop)))))
  
  request-channel)

;; ip.port -> peer functions
(define/contract (lookup-peer peers hostname portnum)
  (peersHT/c string? port/c . -> . cpointer?)
  (hash-ref peers (cons (enet-address-get-host-binary hostname) portnum) (λ () #f)))

(define/contract (register-peer peers  peer*)
  (peersHT/c cpointer? . -> . void)
  (hash-set! peers (peer->host/port peer*) peer*))

(define/contract (unregister-peer peers peer*)
  (peersHT/c cpointer? . -> . void)
  (hash-remove! peers (peer->host/port peer*)))

;; ip.port -> list of message functions
(define/contract (suspend-message susps hostname portnum packet)
  (suspmessageHT/c string? port/c cpointer? . -> . void)
  ;; for all entries in susps a given ip.port is either
  ;; 1. not present
  ;; 2. a nonempty list
  (hash-update! susps (cons (enet-address-get-host-binary hostname) portnum)
                (λ (pkts) (cons packet pkts))
                (λ () (list packet))))

(define/contract (get-susp-messages/clear-entry! susps addr)
  (suspmessageHT/c cpointer? . -> . (listof cpointer?))
  (define key (addr->host/port addr))
  (let ([msgs (hash-ref susps key (λ () '()))])
    (hash-remove! susps key)
    msgs))

(define/contract (preprocess-message data)
  (bytes? . -> . any/c)
  data)