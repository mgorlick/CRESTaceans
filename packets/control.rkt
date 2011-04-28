#lang typed/racket

(require "util.rkt"
         "packets.rkt")

(provide (all-defined-out))

;; -------------
;; SERIALIZATION
;; -------------
(: packet->bytes (ControlPacket -> Bytes))
(define (packet->bytes p)
  (bytes-append (typeline-bytes p) (additional-bytes p) 
                (timestamp-bytes p) (destID-bytes p) 
                (controlinfo-bytes p)))

; line 1 of the control header packet:
; 1 bit set to 1 + 15 bits of ctrl code + 16 bits reserved
(: typeline-bytes (ControlPacket -> Bytes))
(define (typeline-bytes p)
  (define ctrlcode (cond [(Handshake? p) #x0]
                         [(KeepAlive? p) #x1]
                         [(ACK? p) #x2]
                         [(NAK? p) #x3]
                         [(Shutdown? p) #x5]
                         [(ACK2? p) #x6] 
                         [(DropReq? p) #x7]))
  (bytes-append (i->b (16thbiton ctrlcode) 2) #"\0\0"))

; line 2 of the control header packet: depends on packet type
(: additional-bytes (ControlPacket -> Bytes))
(define (additional-bytes p)
  (cond [(ACK? p) (make32 (LightACK-ACKNo p))]
        [(ACK2? p) (make32 (ACK2-ACKNo p))]
        [(DropReq? p) (make32 (DropReq-messageID p))]
        [else #"\0\0\0\0"]))

; line 3 of the control header packet
(: timestamp-bytes (ControlPacket -> Bytes))
(define (timestamp-bytes p) (make32 (Packet-stamp p)))

; line 4 of the control header packet
(: destID-bytes (ControlPacket -> Bytes))
(define (destID-bytes p) (make32 (Packet-destID p)))

(: controlinfo-bytes (ControlPacket -> Bytes))
(define (controlinfo-bytes p)
  (match p
    [(NAK _ _ li) (bytes/32bit li)]
    [(DropReq _ _ _ f l) (bytes/32bit f l)]
    [(FullACK _ _ _ ls rtt rttv abb rr lc) (bytes/32bit ls rtt rttv abb rr lc)]
    [(MedACK _ _ _ ls rtt rttv) (bytes/32bit ls rtt rttv)]
    [(Handshake _ _ u sockt isq m1 m2 cont ch syn)
     (bytes/32bit u (SType->Nat sockt) isq m1 m2 (CType->Nat cont) ch syn)]
    [(or (KeepAlive _ _) (Shutdown _ _) (ACK2 _ _ _) (LightACK _ _ _)) #""]))

;; ---------------
;; DESERIALIZATION
;; ---------------
(: bytes->packet (Bytes -> ControlPacket))
(define (bytes->packet b)
  (match (16thbitoff (integer-bytes->integer b #f #t 0 2))
    [#x0 (deserialize/handshake b)]
    [#x1 (deserialize/keepalive b)]
    [#x2 (deserialize/ack b)]
    [#x3 (deserialize/nak b)]
    [#x5 (deserialize/shutdown b)]
    [#x6 (deserialize/ack2 b)]
    [#x7 (deserialize/dropreq b)]))

;; the `additional info' field in the UDT control header
(: additional (Bytes -> Natural))
(define (additional b) (take32 b 4))

;; the `timestamp' field in the UDT control header
(: timestamp (Bytes -> Natural))
(define (timestamp b) (take32 b 8))

;; the `destination socket ID' field in the UDT control header
(: destid (Bytes -> Natural))
(define (destid b) (take32 b 12))

;; all packets must be at least 128 bits long
(: lacks-full-header? (Bytes -> Boolean))
(define (lacks-full-header? b) (< (bytes-length b) 16))

(: deserialize/keepalive (Bytes -> KeepAlive))
(define (deserialize/keepalive b)
  (cond [(lacks-full-header? b) (raise-parse-error "Invalid keep-alive length")]
        [else (KeepAlive (timestamp b) (destid b))]))

(: deserialize/shutdown (Bytes -> Shutdown))
(define (deserialize/shutdown b)
  (cond [(lacks-full-header? b) (raise-parse-error "Invalid shutdown length")]
        [else (Shutdown (timestamp b) (destid b))]))

(: deserialize/nak (Bytes -> NAK))
(define (deserialize/nak b)
  (cond [(< (bytes-length b) 20) (raise-parse-error "Invalid nak length")]
        [else (NAK (timestamp b) (destid b) (take32 b 16))]))

(: deserialize/ack2 (Bytes -> ACK2))
(define (deserialize/ack2 b)
  (cond [(lacks-full-header? b) (raise-parse-error "Invalid ack2 length")]
        [else (ACK2 (timestamp b) (destid b) (additional b))]))

(: deserialize/dropreq (Bytes -> DropReq))
(define (deserialize/dropreq b)
  (cond [(< (bytes-length b) 24) (raise-parse-error "Invalid drop request length")]
        [else (DropReq (timestamp b) (destid b) (additional b) (take32 b 16) (take32 b 20))]))

(: deserialize/handshake (Bytes -> Handshake))
(define (deserialize/handshake b)
  (cond [(< (bytes-length b) 48) (raise-parse-error "Invalid handshake packet length")]
        [else (Handshake (timestamp b) (destid b) (take32 b 16) (Nat->SType (take32 b 20))
                         (take32 b 24) (take32 b 28) (take32 b 32)
                         (Nat->CType (take32 b 36)) (take32 b 40) (take32 b 44))]))

(: deserialize/ack (Bytes -> ACK))
(define (deserialize/ack b)
  (cond [(lacks-full-header? b) (raise-parse-error "Invalid ack length")]
        [(< (bytes-length b) 28) (LightACK (timestamp b) (destid b) (additional b))]
        [(< (bytes-length b) 40) (MedACK (timestamp b) (destid b) (additional b)
                                         (take32 b 16) (take32 b 20) (take32 b 24))]
        [else (FullACK (timestamp b) (destid b) (additional b)
                       (take32 b 16) (take32 b 20) (take32 b 24)
                       (take32 b 28) (take32 b 32) (take32 b 36))]))