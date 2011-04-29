#lang typed/racket

(require "util.rkt")

(provide (all-defined-out))

(struct: Packet ([stamp : Natural]
                 [destID : Natural]) #:transparent)

; line 3 of the header packet
(: timestamp-bytes (Packet -> Bytes))
(define (timestamp-bytes p) (make32 (Packet-stamp p)))

; line 4 of the header packet
(: destid-bytes (Packet -> Bytes))
(define (destid-bytes p) (make32 (Packet-destID p)))

;; the `timestamp' field in the UDT header
(: timestamp (Bytes -> Natural))
(define (timestamp b) (take32 b 8))

;; the `destination socket ID' field in the UDT header
(: destid (Bytes -> Natural))
(define (destid b) (take32 b 12))

;; all packets must be at least 128 bits long
(: lacks-full-header? (Bytes -> Boolean))
(define (lacks-full-header? b) (< (bytes-length b) 16))

;;; ------------
;;; DATA PACKETS
;;; ------------

(define-type Posn (U 'First 'Middle 'Last 'Only))

(struct: DataPacket Packet ([seqNo : Natural] ; 31 bits
                            [msgNo : Natural] ; 29 bits
                            [posn : Posn]
                            [inOrder? : Boolean]
                            [body : Bytes]) #:transparent)

(struct: FstPacket DataPacket () #:transparent)
(struct: MidPacket DataPacket () #:transparent)
(struct: LstPacket DataPacket () #:transparent)

(struct: SinglePacket DataPacket () #:transparent)

(define make-DataPacket DataPacket)

;;; ---------------
;;; CONTROL PACKETS
;;; ---------------

(define-type ControlPacket (U Handshake KeepAlive ACK ACK2 NAK
                              Shutdown DropReq))

(define-type SocketType (U 'Stream 'Dgram))

(: SType->Nat (SocketType -> Natural))
(define (SType->Nat s)
  (cond [(equal? s 'Stream) 0]
        [(equal? s 'Dgram) 1]))

(: Nat->SType (Natural -> SocketType))
(define (Nat->SType i)
  (cond [(equal? i 0) 'Stream]
        [(equal? i 1) 'Dgram]
        [else (raise-parse-error (format "Invalid value for socket type: ~a" i))]))

(define-type ConnectionType (U 'Deny 'Accept 'CSReq 'RDVReq))

(: CType->Nat (ConnectionType -> Natural))
(define (CType->Nat c)
  (cond [(equal? c 'Deny) 0]
        [(equal? c 'Accept) 1]
        [(equal? c 'CSReq) 2]
        [(equal? c 'RDVReq) 3]))

(: Nat->CType (Natural -> ConnectionType))
(define (Nat->CType i)
  (cond [(equal? i 0) 'Deny]
        [(equal? i 1) 'Accept]
        [(equal? i 2) 'CSReq]
        [(equal? i 3) 'RDVReq]
        [else (raise-parse-error (format "Invalid value for connection type: ~a" i))]))

(struct: Shutdown Packet () #:transparent)

(struct: KeepAlive Packet () #:transparent)

(struct: Handshake Packet ([udtVersion : Natural]
                           [socketType : SocketType]
                           [initSeqNo : Natural]
                           [maxPacketSize : Natural]
                           [maxFlowWindowSize : Natural]
                           [connectionType : ConnectionType]
                           [channelID : Natural]
                           [SYNcookie : Natural]) #:transparent)
#|[peerIP : Natural] 
XXX fixme skipping for now, 
like Java impl) #:transparent) |#

(struct: DropReq Packet ([messageID : Natural]
                         [firstSeqNo : Natural]
                         [lastSeqNo : Natural]) #:transparent)

(struct: ACK2 Packet ([ACKNo : Natural]) #:transparent)

(struct: NAK Packet ([lossInfo : Natural]) #:transparent) ; XXX array of ints

(struct: LightACK Packet ([ACKNo : Natural]) #:transparent) ; XXX also has lseqno

(struct: MedACK Packet ([ACKNo : Natural]
                        [lastSeqNo : Natural]
                        [RTT : Natural]
                        [RTTVariance : Natural]) #:transparent)

(struct: FullACK Packet ([ACKNo : Natural]
                         [lastSeqNo : Natural]
                         [RTT : Natural]
                         [RTTVariance : Natural]
                         [availBuffBytes : Natural]
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