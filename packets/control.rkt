#lang typed/racket

(require "util.rkt"
         "packets.rkt")

(provide (prefix-out control/ (all-defined-out)))

;; UTILITIES
;; ---------
(: 16thbiton (Integer -> Integer))
(define (16thbiton n) ($ n (<< 1 15)))

(: 16thbitoff (Integer -> Integer))
(define (16thbitoff n) (& n (~ (<< 1 15))))

(define i->b
  (case-lambda:
   {([n : Integer] [size : Natural])
    (integer->integer-bytes n size #f #t)}
   {([n : Integer] [size : Natural] [signed? : Boolean])
    (integer->integer-bytes n size signed? #t)}))

(define b->i
  (case-lambda:
   {([b : Bytes])
    (integer-bytes->integer b #f #t)}
   {([b : Bytes] [signed? : Boolean])
    (integer-bytes->integer b signed? #t)}))

;; -------------
;; SERIALIZATION
;; -------------
(: packet->bytes (ControlPacket -> Bytes))
(define (packet->bytes p)
  (bytes-append (typeline-bytes p) (additional-bytes p) 
                (timestamp-bytes p) (destID-bytes p) (controlinfo-bytes p)))

; line 1 of the control header packet:
; 1 bit set to 1 + 15 bits of ctrl code + 16 bits reserved
(: typeline-bytes (ControlPacket -> Bytes))
(define (typeline-bytes p)
  (define ctrlcode (cond [(Handshake? p) #x0] [(KeepAlive? p) #x1]
                         [(ACK? p) #x2] [(NAK? p) #x3] [(Shutdown? p) #x5]
                         [(ACK2? p) #x6] [(DropReq? p) #x7]))
  (bytes-append (i->b (16thbiton ctrlcode) 2) #"\0\0"))

; line 2 of the control header packet: depends on packet type
(: additional-bytes (ControlPacket -> Bytes))
(define (additional-bytes p)
  (cond [(ACK? p) (i->b (LightACK-ACKNo p) 4)]
        [(ACK2? p) (i->b (ACK2-ACKNo p) 4)]
        [(DropReq? p) (i->b (DropReq-messageID p) 4)]
        [else #"\0\0\0\0"]))

; line 3 of the control header packet
(: timestamp-bytes (ControlPacket -> Bytes))
(define (timestamp-bytes p) (i->b (Packet-stamp p) 4))

; line 4 of the control header packet
(: destID-bytes (ControlPacket -> Bytes))
(define (destID-bytes p) (i->b (Packet-destID p) 4))

(: controlinfo-bytes (ControlPacket -> Bytes))
(define (controlinfo-bytes p)
  (cond [(or (KeepAlive? p) (Shutdown? p) (ACK2? p) (LightACK? p)) #""]
        [(NAK? p) (i->b (NAK-lossInfo p) 4)]
        [(DropReq? p) (bytes-append (i->b (DropReq-firstSeqNo p) 4)
                                    (i->b (DropReq-lastSeqNo p) 4))]
        [(MedACK? p) (bytes-append (i->b (MedACK-lastSeqNo p) 4)
                                   (i->b (MedACK-RTT p) 4)
                                   (i->b (MedACK-RTTVariance p) 4))]
        [(FullACK? p) (bytes-append (i->b (MedACK-lastSeqNo p) 4)
                                    (i->b (MedACK-RTT p) 4)
                                    (i->b (MedACK-RTTVariance p) 4)
                                    (i->b (FullACK-availBuffBytes p) 4)
                                    (i->b (FullACK-receiveRate p) 4)
                                    (i->b (FullACK-linkCap p) 4))]
        [(Handshake? p)
         (match p
           [(Handshake _ _ u s i m1 m2 ct ch syn pip)
            (bytes-append (i->b u 4) (i->b (SocketType->Nat s) 4)
                          (i->b m1 4) (i->b m2 4) (i->b ct 4 #t)
                          (i->b ch 4) (i->b syn 4) (i->b pip 16))])]))

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

(: raise-parse-error (String -> Nothing))
(define (raise-parse-error str)
  (raise (make-exn:fail str (current-continuation-marks))))

(: natcheck (Number -> Natural))
(define (natcheck v)
  (if (exact-nonnegative-integer? v) v
      (raise-parse-error "Found negative number where natural should have been")))

;; take32: take 32 bits of data starting at `s' and 
;; convert data to natural number. raise exn if not natural number
(: take32 (Bytes Natural -> Natural))
(define (take32 b s) (natcheck (b->i (subbytes b s (+ 4 s)))))
(: take32/signed (Bytes Natural -> Integer))
(define (take32/signed b s) (b->i (subbytes b s (+ 4 s)) #t))

;; take128: take 128 bits of data starting at `s' and 
;; convert data to natural number. raise exn if not natural number
(: take128 (Bytes Natural -> Natural))
(define (take128 b s) (natcheck (b->i (subbytes b s (+ 16 s)))))

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

(: deserialize/handshake (Bytes -> Handshake))
(define (deserialize/handshake b)
  (cond [(< (bytes-length b) 64) ; header + ip + 8x32bit fields
         (raise-parse-error "Invalid handshake packet length")]
        [else (Handshake (timestamp b) (destid b) (take32 b 16) 
                         (Nat->SocketType (take32 b 20))
                         (take32 b 24) (take32 b 28) (take32 b 32)
                         (take32/signed b 36) (take32 b 40) (take32 b 44)
                         (take128 b 48))]))

(: deserialize/keepalive (Bytes -> KeepAlive))
(define (deserialize/keepalive b)
  (cond [(lacks-full-header? b) (raise-parse-error "Invalid keep-alive length")]
        [else (KeepAlive (timestamp b) (destid b))]))

(: deserialize/shutdown (Bytes -> Shutdown))
(define (deserialize/shutdown b)
  (cond [(lacks-full-header? b) (raise-parse-error "Invalid shutdown length")]
        [else (Shutdown (timestamp b) (destid b))]))

(: deserialize/ack (Bytes -> ACK))
(define (deserialize/ack b)
  (cond [(lacks-full-header? b) ; not big enough for light
         (raise-parse-error "Invalid ack length")]
        [(< (bytes-length b) 28) ; not big enough for med
         (LightACK (timestamp b) (destid b) (additional b))]
        [(< (bytes-length b) 40) ; not big enough for full
         (MedACK (timestamp b) (destid b) (additional b)
                 (take32 b 16) (take32 b 20) (take32 b 24))]
        [else 
         (FullACK (timestamp b) (destid b) (additional b)
                  (take32 b 16) (take32 b 20) (take32 b 24)
                  (take32 b 28) (take32 b 32) (take32 b 36))]))

(: deserialize/nak (Bytes -> NAK))
(define (deserialize/nak b)
  (cond
    [(< (bytes-length b) 20) (raise-parse-error "Invalid nak length")]
    [else (NAK (timestamp b) (destid b) (take32 b 16))]))

(: deserialize/ack2 (Bytes -> ACK2))
(define (deserialize/ack2 b)
  (cond
    [(< (bytes-length b) 20) (raise-parse-error "Invalid ack2 length")]
    [else (ACK2 (timestamp b) (destid b) (additional b))]))

(: deserialize/dropreq (Bytes -> DropReq))
(define (deserialize/dropreq b)
  (cond
    [(< (bytes-length b) 24) (raise-parse-error "Invalid drop request length")]
    [else(DropReq (timestamp b) (destid b) (additional b) 
                  (take32 b 16) (take32 b 20))]))

(define sp (Shutdown 5 5))
(define kap (KeepAlive (abs (sub1 (expt 2 32))) 55555))
(time (bytes->packet (packet->bytes sp)))
(time (bytes->packet (packet->bytes kap)))