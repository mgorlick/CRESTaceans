#lang typed/racket

(require "util.rkt"
         "packets.rkt")

(provide (all-defined-out))

;; -------------
;; SERIALIZATION
;; -------------
(: cpacket->bytes (ControlPacket Bytes -> Natural))
(define (cpacket->bytes p buf)
  (define info (controlinfo-bytes p))
  (bytes-copy! buf 0 (typeline-bytes p))
  (bytes-copy! buf 2 #"\0\0")
  (bytes-copy! buf 4 (additional-bytes p))
  (bytes-copy! buf 8 (timestamp-bytes p))
  (bytes-copy! buf 12 (destid-bytes p))
  (bytes-copy! buf 16 info)
  (+ 16 (bytes-length info)))

; line 1 of the control header packet:
; 1 bit set to 1 + 15 bits of ctrl code + 16 bits reserved
(: typeline-bytes (ControlPacket -> Bytes))
(define (typeline-bytes p)
  (define: ctrlcode : Natural (cond [(Handshake? p) #x0]
                                    [(KeepAlive? p) #x1]
                                    [(ACK? p) #x2]
                                    [(NAK? p) #x3]
                                    [(Shutdown? p) #x5]
                                    [(ACK2? p) #x6] 
                                    [(DropReq? p) #x7]))
  (integer->integer-bytes (biton 15 ctrlcode) 2 #f #t))

; line 2 of the control header packet: depends on packet type
(: additional-bytes (ControlPacket -> Bytes))
(define (additional-bytes p)
  (match p
    [(LightACK _ _ ackno _) (make32 ackno)]
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
    [(LightACK _ _ _ ls) (bytes/32bit ls)]
    [(Handshake _ _ u st isq m1 m2 ct ch syn) (bytes/32bit u (SType->Nat st) isq m1 m2 (CType->Nat ct) ch syn)]
    [_ #""]))

;; ---------------
;; DESERIALIZATION
;; ---------------
(: bytes->cpacket (Bytes Natural -> ControlPacket))
(define (bytes->cpacket b amt)
  (match (type b)
    [#x0 (deserialize/handshake b amt)]
    [#x1 (deserialize/keepalive b amt)]
    [#x2 (deserialize/ack b amt)]
    [#x3 (deserialize/nak b amt)]
    [#x5 (deserialize/shutdown b amt)]
    [#x6 (deserialize/ack2 b amt)]
    [#x7 (deserialize/dropreq b amt)]))

(: type (Bytes -> Integer))
(define (type b) (bitoff 15 (integer-bytes->integer b #f #t 0 2)))

;; the `additional info' field in the UDT control header
(: additional (Bytes -> Natural))
(define (additional b) (take32 b 4))

(: deserialize/keepalive (Bytes Natural -> KeepAlive))
(define (deserialize/keepalive b amt)
  (cond [(< amt 16) (raise-parse-error "Invalid keep-alive length")]
        [else (KeepAlive (timestamp b) (destid b))]))

(: deserialize/shutdown (Bytes Natural -> Shutdown))
(define (deserialize/shutdown b amt)
  (cond [(< amt 16) (raise-parse-error "Invalid shutdown length")]
        [else (Shutdown (timestamp b) (destid b))]))

(: deserialize/nak (Bytes Natural -> NAK))
(define (deserialize/nak b amt)
  (: acks (Bytes -> (Listof Natural)))
  (define (acks b)
    (let: loop : (Listof Natural) ([c : Natural 16])
      (if (< amt (+ c 4))
          empty
          (cons (take32 b c) (loop (+ c 4))))))
  (cond [(< amt 20) (raise-parse-error "Invalid nak length")]
        [else (NAK (timestamp b) (destid b) (acks b))]))

(: deserialize/ack2 (Bytes Natural -> ACK2))
(define (deserialize/ack2 b amt)
  (cond [(< amt 16) (raise-parse-error "Invalid ack2 length")]
        [else (ACK2 (timestamp b) (destid b) (additional b))]))

(: deserialize/dropreq (Bytes Natural -> DropReq))
(define (deserialize/dropreq b amt)
  (cond [(< amt 24) (raise-parse-error "Invalid drop request length")]
        [else (DropReq (timestamp b) (destid b) (additional b) (take32 b 16) (take32 b 20))]))

(: deserialize/handshake (Bytes Natural -> Handshake))
(define (deserialize/handshake b amt)
  (cond [(< amt 48) (raise-parse-error "Invalid handshake packet length")]
        [else (Handshake (timestamp b) (destid b) (take32 b 16) (Nat->SType (take32 b 20))
                         (take32 b 24) (take32 b 28) (take32 b 32)
                         (Nat->CType (take32 b 36)) (take32 b 40) (take32 b 44))]))

(: deserialize/ack (Bytes Natural -> ACK))
(define (deserialize/ack b amt)
  (cond [(< amt 20) (raise-parse-error "Invalid ack length")]
        [(< amt 28) (LightACK (timestamp b) (destid b) (additional b) (take32 b 16))]
        [(< amt 40) (MedACK (timestamp b) (destid b) (additional b)
                                         (take32 b 16) (take32 b 20) (take32 b 24))]
        [else (FullACK (timestamp b) (destid b) (additional b)
                       (take32 b 16) (take32 b 20) (take32 b 24)
                       (take32 b 28) (take32 b 32) (take32 b 36))]))