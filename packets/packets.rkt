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

(struct: Handshake Packet ([udtVersion : Natural]
                           [socketType : SocketType]
                           [initSeqNo : Natural]
                           [maxPacketSize : Natural]
                           [maxFlowWindowSize : Natural]
                           [connectionType : Integer]
                           [channelID : Natural]
                           [SYNcookie : Natural]
                           [peerIP : Natural] ; 128 bits
                           ) #:transparent)

(struct: KeepAlive Packet ())

(struct: LightACK Packet ([ACKNo : Natural]))
(struct: MedACK LightACK ([lastSeqNo : Natural]
                          [RTT : Natural]
                          [RTTVariance : Natural]))
(struct: FullACK MedACK ([availBuffBytes : Natural]
                         [receiveRate : Natural]
                         [linkCap : Natural]))
(define-type ACK (U LightACK MedACK FullACK))
(define-predicate ACK? ACK)

(struct: NAK Packet ([lossInfo : Natural]))

(struct: ACK2 Packet ([ACKNo : Natural]))

(struct: DropReq Packet ([messageID : Natural]
                         [firstSeqNo : Natural]
                         [lastSeqNo : Natural]))

(struct: Shutdown Packet ())

;; XXX Fixme implement user defined control packet