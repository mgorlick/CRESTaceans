#lang racket

(require ffi/unsafe
         "../ctypes.rkt")
(provide (except-out (all-defined-out)
                     lib
                     defenet+
                     defenet
                     defenet*)
         cpointer?)

;; source distribution version: 1.3.2
(define lib (ffi-lib "libenet"))

(define-syntax-rule (defenet+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) lib typ)))
(define-syntax-rule (defenet obj typ)
  (defenet+ obj obj typ))
(define-syntax-rule (defenet* typ obj ...)
  (begin (defenet obj typ) ...))

(define _enet-bool (make-ctype _int
                               (λ (rktval) (if rktval 0 -1))
                               (λ (cval) (zero? cval))))
(define (ptr/or-null y/n/f ptr)
  (if (> y/n/f 0) ptr #f))

(define-cpointer-type _ENetHost-pointer)

(define _ENetEventType (_enum '(none = 0 connect = 1 disconnect = 2 receive = 3)))
(define _ENetSocketType (_enum '(stream = 1 datagram = 2)))
(define _ENetSocketOption (_enum '(nonblock = 1 broadcast = 2 rcvbuf = 3 sndbuf = 4 reuseaddr = 5)))
(define _ENetSocketWait (_bitmask '(none = 0 send = 1 receive = 2) _uint32))
(define _ENetPacketFlag (_bitmask '(reliable = 1 unsequenced = 2 no-allocate = 4 unreliable-fragment = 8) _uint32))

(define-cstruct _ENetAddress ([host _uint32] [port _uint16]))
(define-cstruct _ENetBuffer ([data _pointer] [dataLength _size_t]))
(define-cstruct _ENetListNode ([next _ENetListNode-pointer]
                               [previous _ENetListNode-pointer]))
(define-cstruct _ENetPacket ([refcount _size_t]
                             [flags _ENetPacketFlag]
                             [data _pointer]
                             [dataLength _size_t]
                             [freecallback _pointer]))
(define-cstruct _ENetPeer ([dispatchList _ENetListNode]
                           [host _ENetHost-pointer]
                           [outgoingPeerID _uint16]
                           [incomingPeerID _uint16]
                           [connectID _uint32]
                           [outgoingSessionID _uint8]
                           [incomingSessionID _uint8]
                           [address _ENetAddress]
                           #| XXX many more fields |# ))
(define-cstruct _ENetEvent ([type _ENetEventType]
                            [peer _ENetPeer-pointer]
                            [channelID _uint8]
                            [data _uint32]
                            [packet _ENetPacket-pointer]))

(define ENet-Host-Any 0)
(define ENet-Host-Broadcast #xFFFFFFFF)
(define ENet-Port-Any 0)

;; General
(defenet enet-initialize (_fun -> _enet-bool))
(defenet enet-deinitialize (_fun -> _void))

;; Host
(define ENetMaxPeerCount (sub1 (expt 2 12))) ; any higher than this and enet_host_create returns null
(defenet enet-host-create (_fun (_or-null _ENetAddress-pointer)
                                _size_t _size_t _uint32 _uint32 -> _ENetHost-pointer))
(defenet enet-host-destroy (_fun _ENetHost-pointer -> _void))
(defenet enet-host-flush (_fun _ENetHost-pointer -> _void))
(defenet enet-host-service (_fun _ENetHost-pointer (e : (_ptr o _ENetEvent)) _uint32 -> (r : _int)
                                 -> (ptr/or-null r e)))
(defenet enet-host-check-events (_fun _ENetHost-pointer (e : (_ptr o _ENetEvent)) -> (r : _int)
                                      -> (ptr/or-null r e)))
(defenet enet-host-connect (_fun _ENetHost-pointer _ENetAddress-pointer _size_t _uint32 -> _ENetPeer-pointer))
(defenet enet-host-broadcast (_fun _ENetHost-pointer _uint8 _ENetPacket-pointer -> _void))
(defenet enet-host-bandwidth-limit (_fun _ENetHost-pointer _uint32 _uint32 -> _void))
(defenet enet-host-channel-limit (_fun _ENetHost-pointer _size_t -> _void))

;; Peer
(defenet enet-peer-send (_fun _ENetPeer-pointer _uint8 _ENetPacket-pointer -> _enet-bool))
(defenet enet-peer-receive (_fun _ENetPeer-pointer (channelID : (_ptr o _uint8)) -> (packet : _ENetPacket-pointer)
                                 -> (values packet channelID)))
(defenet enet-peer-disconnect (_fun _ENetPeer-pointer _uint32 -> _void))
(defenet enet-peer-disconnect-later (_fun _ENetPeer-pointer _uint32 -> _void))
(defenet enet-peer-disconnect-now (_fun _ENetPeer-pointer _uint32 -> _void))

;; Address
(defenet enet-address-set-host (_fun _ENetAddress-pointer _string -> _enet-bool))
(defenet+ enet-address-get-hostnum
  enet-address-set-host (_fun (name) :: 
                              (v : (_ptr o _ENetAddress))
                              (name : _string)
                              -> (r : _enet-bool)
                              -> (and r (ENetAddress-host v))))

(define enet-address-destroy free)

(define (host/port->addr host port)
  (define addr (make-ENetAddress 0 port))
  (set-ENetAddress-port! addr port)
  (if (enet-address-set-host addr host)
      addr
      #f))

(define (addr->host/port addr)
  (cons (ENetAddress-host addr) (ENetAddress-port addr)))

(define (peer->host/port peer)
  (addr->host/port (ENetPeer-address peer)))

;; Packets
(defenet+ enet-packet-create/bytes enet-packet-create
  (_fun (pkt flags) ::
        (pkt : _bytes)
        (_size_t = (bytes-length pkt))
        (flags : _ENetPacketFlag)
        -> _ENetPacket-pointer))
(defenet enet-packet-destroy (_fun _ENetPacket-pointer -> _void))

(define (get-packet-data p)
  (cast (ENetPacket-data p) _pointer (_bytes o (ENetPacket-dataLength p))))

(define (copy-packet-data p)
  (bytes-copy (get-packet-data p)))

(define enet-on? (enet-initialize))