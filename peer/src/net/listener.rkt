#lang racket

(require "../../../bindings/enet/enet.rkt"
         (planet bzlib/thread)
         racket/async-channel)

(provide run-listener)

(define port/c (and/c exact-integer? (between/c 0 65535)))
(define peersHT/c (hash/c (cons/c exact-nonnegative-integer? port/c) cpointer?))
(define suspmessageHT/c (hash/c (cons/c exact-nonnegative-integer? port/c) (listof cpointer?)))

(define/contract (run-listener host port message-on-channel)
  (string? port/c async-channel? . -> . void)
  (define addr (host/port->addr host port))
  
  (define enet-host (enet-host-create addr ENetMaxPeerCount 0 0 0))
  (define enet-host-mutex (make-semaphore 1))
  
  ;; it's useful to protect the host and peer table with separate mutexes since
  ;; there are some operations (e.g., reponding to a disconnect, or receiving)
  ;; that don't modify the host
  
  ;; this mutex protects both peers HT and suspended messages HT
  (define enet-peers-mutex (make-semaphore 1))
  
  ;; map (ip . port) cons cells to connected ENetPeer *ptrs
  ;; XXX fixme problem with this design: is not going to scale well with the number of peers connected
  ;; since it requires a lock on the whole table to modify the particulars of any one peer.
  ;; it's the way it is now because it was much easier to implement. change when able
  (define/contract enet-peers peersHT/c (make-hash))
  
  ;; map delayed (ip . port) cons cells to lists of ENetPacket *ptrs intended
  ;; for the corresponding ENetPeer in the enet-peers table, once that peer connects
  ;; we have to handle messages submitted pre-connect this way, because
  ;; enet will drop the message silently if we call enet-host-connect and
  ;; call enet-peer-send immediately afterward
  ;; this doesn't need a mutex since every modification instance already requires a lock on peer-mutex
  (define/contract susp-messages suspmessageHT/c (make-hash))
  
  (define event-loop
    (thread
     (λ ()
       (let loop ()
         (let ([event (call-with-semaphore enet-host-mutex enet-host-service #f enet-host 0)])
           (when event
             (case (ENetEvent-type event)
               ['connect (semaphore-wait enet-host-mutex) ; enet-peer-send modifies host data
                         (semaphore-wait enet-peers-mutex) ; enet-peer-send modifies peer data
                         (define peer (ENetEvent-peer event))
                         ;; clear the peer's entry in suspended messages table, if present
                         (define msgs (get-susp-messages/clear-entry! susp-messages (ENetPeer-address peer)))
                         ;; move the completed peer to the connected peer table
                         (register-peer enet-peers peer)
                         (foldr (λ (pkt v) (enet-peer-send peer 0 pkt) v) #f msgs) ;; foldr preserves ordering
                         (semaphore-post enet-peers-mutex)
                         (semaphore-post enet-host-mutex)
                         (printf "Peer connected~n")]
               
               ['disconnect (semaphore-wait enet-peers-mutex)
                            (unregister-peer enet-peers (ENetEvent-peer event))
                            (semaphore-post enet-peers-mutex)
                            (printf "Peer disconnected~n")]
               
               ['receive (async-channel-put message-on-channel
                                            (preprocess-message
                                             (copy-packet-data
                                              (ENetEvent-packet event))))])))
         (loop)))))
  
  (define/contract (send-message host port data)
    (string? port/c bytes? . -> . void)
    (semaphore-wait enet-host-mutex)
    (semaphore-wait enet-peers-mutex)
    ; cannot unlock peers mutex until message sent or queued
    ; since otherwise the following interleaving could happen (thread A = sender, thread B = receiver):
    ; A: lock peer, look up peer -- not found, unlock peer
    ; B: peer connected, lock peer, lock susp, empty susp table entry, unlock susp
    ; A: lock susp, queue up susp message, unlock susp
    ; B: send out all susp messages for this peer, unlock peer
    (let ([peer (lookup-peer enet-peers (host/port->addr host port))])
      (if peer
          (enet-peer-send peer 0 (enet-packet-create/bytes data 'reliable))
          ; peer not connected yet. queue up the message in the racket-level queue
          ; (don't submit it to enet yet since enet will silently throw it away before the peer finishes connection)
          (let ([addr (host/port->addr host port)]
                [pkt (enet-packet-create/bytes data 'reliable)])
            (when addr
              (enet-host-connect enet-host addr 1 0)
              (suspend-message susp-messages addr pkt)))))
    (semaphore-post enet-peers-mutex)
    (semaphore-post enet-host-mutex))
  
  ;; loop looking through mailbox for outgoing messages  
  (let loop ()
    (receive/match
      [(list 'send (? string? host) (? exact-nonnegative-integer? port) (? bytes? data))
       (send-message host port data)]
      
      [a (printf "listener dropping message ~a~n" a)])
    (loop)))

;; ip.port -> peer functions
(define/contract (lookup-peer peers addr)
  (peersHT/c cpointer? . -> . cpointer?)
  (hash-ref peers (addr->host/port addr) (λ () #f)))

(define/contract (register-peer peers  peer*)
  (peersHT/c cpointer? . -> . void)
  (hash-set! peers (peer->host/port peer*) peer*))

(define/contract (unregister-peer peers peer*)
  (peersHT/c cpointer? . -> . void)
  (hash-remove! peers (peer->host/port peer*)))

;; ip.port -> list of message functions
(define/contract (suspend-message susps addr packet)
  (suspmessageHT/c cpointer? cpointer? . -> . void)
  ;; for all entries in susps a given ip.port is either
  ;; 1. not present
  ;; 2. a nonempty list
  (hash-update! susps (addr->host/port addr)
                (λ (msgs) (cons packet msgs))
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