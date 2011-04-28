#lang typed/racket

(require "util.rkt"
         "dm.rkt")

(provide (all-defined-out))

(struct: Packet ([stamp : Natural]
                 [destID : Natural]) #:transparent)

;;; ------------
;;; DATA PACKETS
;;; ------------

(define-type Posn (U 'First 'Middle 'Last 'Only))

(struct: DataPacket Packet ([seqNo : Natural] ; 31 bits
                            [msgNo : Natural] ; 29 bits
                            [posn : Posn]
                            [inOrder? : Boolean]) #:transparent)

;;; ---------------
;;; CONTROL PACKETS
;;; ---------------

(define-type ControlPacket (U Handshake KeepAlive ACK ACK2 NAK
                              Shutdown DropReq))

(define-type SocketType (U 'Stream 'Dgram))

(: SocketType->Nat (SocketType -> Natural))
(define/match (SocketType->Nat s)
  ['Stream 0]
  ['Dgram 1])
(: Nat->SocketType (Natural -> SocketType))
(define/match (Nat->SocketType i)
  [0 'Stream]
  [1 'Dgram])

(struct: Shutdown Packet () #:transparent)

(struct: KeepAlive Packet () #:transparent)

(struct: LightACK Packet ([ACKNo : Natural]) #:transparent)

(struct: ACK2 Packet ([ACKNo : Natural]) #:transparent)

(struct: NAK Packet ([lossInfo : Natural]) #:transparent)

(struct: Handshake Packet ([udtVersion : Natural]
                           [socketType : SocketType]
                           [initSeqNo : Natural]
                           [maxPacketSize : Natural]
                           [maxFlowWindowSize : Natural]
                           [connectionType : Integer] ; XXX fixme convert to union once valid values known
                           [channelID : Natural]
                           [SYNcookie : Natural]) #:transparent)
#|[peerIP : Natural] 
XXX fixme skipping for now, 
like Java impl) #:transparent) |#

(struct: DropReq Packet ([messageID : Natural]
                         [firstSeqNo : Natural]
                         [lastSeqNo : Natural]) #:transparent)

(struct: MedACK LightACK ([lastSeqNo : Natural]
                          [RTT : Natural]
                          [RTTVariance : Natural]) #:transparent)

(struct: FullACK MedACK ([availBuffBytes : Natural]
                         [receiveRate : Natural]
                         [linkCap : Natural]) #:transparent)

(define-type ACK (U LightACK MedACK FullACK))
(define-predicate ACK? ACK)

(define make-Handshake Handshake)
(define make-Shutdown Shutdown)
(define make-KeepAlive KeepAlive)
(define make-LightACK LightACK)
(define make-MedACK MedACK)
(define make-FullACK FullACK)
(define make-NAK NAK)
(define make-ACK2 ACK2)
(define make-DropReq DropReq)

;; XXX Fixme implement user defined control packet