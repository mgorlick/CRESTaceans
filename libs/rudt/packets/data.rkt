#lang typed/racket

(require "util.rkt"
         "packets.rkt")

(provide dpacket->bytes
         bytes->dpacket)

;; -------------
;; SERIALIZATION
;; -------------
(: dpacket->bytes (DataPacket Bytes -> Natural))
(define (dpacket->bytes p buffer)
  (write-seqno-bytes! p buffer)
  (write-msgno-bytes! p buffer)
  (write-timestamp-bytes! p buffer)
  (write-destid-bytes! p buffer)
  (bytes-copy! buffer 16 (DataPacket-body p))
  (+ 16 (bytes-length (DataPacket-body p))))

;; functional versions
(: seqno-bytes (DataPacket -> Bytes))
(define (seqno-bytes p) (make32 (natcheck (bitoff 31 (DataPacket-seqNo p)))))
(: msgno-bytes (DataPacket -> Bytes))
(define (msgno-bytes p) (make32 (setbit/posn p (setbit/ordered (DataPacket-inOrder? p) (DataPacket-msgNo p)))))

;; imperative versions
(: write-seqno-bytes! (DataPacket Bytes -> Bytes))
(define (write-seqno-bytes! p buffer) (write32! (bitoff 31 (DataPacket-seqNo p)) buffer 0))
(: write-msgno-bytes! (DataPacket Bytes -> Bytes))
(define (write-msgno-bytes! p buffer)
  (write32! (setbit/posn p (setbit/ordered (DataPacket-inOrder? p) (DataPacket-msgNo p))) buffer 4))

(: setbit/posn (DataPacket Integer -> Integer))
(define (setbit/posn p n)
  (cond [(FstPacket? p)    (biton  31 (bitoff 30 n))]
        [(MidPacket? p)    (bitoff 31 (bitoff 30 n))]
        [(LstPacket? p)    (bitoff 31 (biton  30 n))]
        [(SinglePacket? p) (biton  31 (biton  30 n))]))

(: setbit/ordered (Boolean Integer -> Integer))
(define (setbit/ordered inorder? n)
  (if inorder?
      (biton  29 n)
      (bitoff 29 n)))

;; ---------------
;; DESERIALIZATION
;; ---------------

(: bytes->dpacket (Bytes Natural -> DataPacket))
(define (bytes->dpacket b amt)
  (cond [(< amt 16) (raise-parse-error "Invalid data packet")]
        [else ((typecons b) 
               (timestamp b) (destid b) (seqno b) (msgno b)
               (bitwise-bit-set? (take32 b 4) 29)
               (subbytes b 16 amt)
               )]))

(: msgno (Bytes -> Natural))
(define (msgno b) (natcheck (bitoff 31 (bitoff 30 (bitoff 29 (take32 b 4))))))

(: seqno (Bytes -> Natural))
(define (seqno b) (natcheck (bitoff 31 (take32 b 0))))

(: typecons (Bytes -> (Natural Natural Natural Natural Boolean Bytes -> DataPacket)))
(define (typecons b)
  (match (bitwise-bit-field (take32 b 4) 30 32)
    [2 FstPacket]
    [0 MidPacket]
    [1 LstPacket]
    [3 SinglePacket]))