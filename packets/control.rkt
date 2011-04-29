#lang typed/racket

(require "util.rkt"
         "packets.rkt")

(provide (all-defined-out))

;; -------------
;; SERIALIZATION
;; -------------
(: cpacket->bytes (ControlPacket -> Bytes))
(define (cpacket->bytes p)
  (bytes-append (typeline-bytes p) (additional-bytes p)
                (timestamp-bytes p) (destid-bytes p) 
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
  (bytes-append (i->b (biton 15 ctrlcode) 2) #"\0\0"))

; line 2 of the control header packet: depends on packet type
(: additional-bytes (ControlPacket -> Bytes))
(define (additional-bytes p)
  (match p
    [(LightACK _ _ ackno) (make32 ackno)]
    [(MedACK _ _ ackno _ _ _) (make32 ackno)]
    [(FullACK _ _ ackno _ _ _ _ _ _) (make32 ackno)]
    [(ACK2 _ _ ackno) (make32 ackno)]
    [(DropReq _ _ mid _ _) (make32 mid)]
    [_ #"\0\0\0\0"]))

(: controlinfo-bytes (ControlPacket -> Bytes))
(define (controlinfo-bytes p)
  (match p
    [(NAK _ _ li) (apply bytes-append (map make32 li))]
    [(DropReq _ _ _ f l) (bytes/32bit f l)]
    [(FullACK _ _ _ ls rtt rttvar abb rr lc) (bytes/32bit ls rtt rttvar abb rr lc)]
    [(MedACK _ _ _ ls rtt rttv) (bytes/32bit ls rtt rttv)]
    [(Handshake _ _ u st isq m1 m2 ct ch syn) (bytes/32bit u (SType->Nat st) isq m1 m2 (CType->Nat ct) ch syn)]
    [_ #""]))

;; ---------------
;; DESERIALIZATION
;; ---------------
(: bytes->cpacket (Bytes -> ControlPacket))
(define (bytes->cpacket b)
  (match (type b)
    [#x0 (deserialize/handshake b)]
    [#x1 (deserialize/keepalive b)]
    [#x2 (deserialize/ack b)]
    [#x3 (deserialize/nak b)]
    [#x5 (deserialize/shutdown b)]
    [#x6 (deserialize/ack2 b)]
    [#x7 (deserialize/dropreq b)]))

(: type (Bytes -> Integer))
(define (type b) (bitoff 15 (integer-bytes->integer b #f #t 0 2)))

;; the `additional info' field in the UDT control header
(: additional (Bytes -> Natural))
(define (additional b) (take32 b 4))

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
  (: nats (Bytes -> (Listof Natural)))
  (define (nats b)
    (let: loop : (Listof Natural) ([c : Natural 16])
      (if (< (bytes-length b) (+ c 4))
          empty
          (cons (take32 b c) (loop (+ c 4))))))
  (cond [(< (bytes-length b) 20) (raise-parse-error "Invalid nak length")]
        [else (NAK (timestamp b) (destid b) (nats b))]))

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