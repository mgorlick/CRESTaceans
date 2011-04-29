#lang typed/racket

(require "util.rkt"
         "packets.rkt")

(provide dpacket->bytes
         bytes->dpacket)

;; -------------
;; SERIALIZATION
;; -------------
(: dpacket->bytes (DataPacket -> Bytes))
(define (dpacket->bytes p)
  (bytes-append (seqno-bytes p) (msgno-bytes p) (timestamp-bytes p) (destid-bytes p) (DataPacket-body p)))

(: seqno-bytes (DataPacket -> Bytes))
(define (seqno-bytes p) (make32 (natcheck (bitoff 31 (DataPacket-seqNo p)))))

(: msgno-bytes (DataPacket -> Bytes))
(define (msgno-bytes p)
  (make32
   (setbit/posn (DataPacket-posn p)
                (setbit/ordered (DataPacket-inOrder? p)
                                (DataPacket-msgNo p)))))

(: setbit/posn (Posn Integer -> Integer))
(define (setbit/posn posn n)
  (match posn
    ['First  (biton  31 (bitoff 30 n))]
    ['Middle (bitoff 31 (bitoff 30 n))]
    ['Last   (bitoff 31 (biton  30 n))]
    ['Only   (biton  31 (biton  30 n))]))

(: setbit/ordered (Boolean Integer -> Integer))
(define (setbit/ordered inorder? n)
  (if inorder?
      (biton  29 n)
      (bitoff 29 n)))

;; ---------------
;; DESERIALIZATION
;; ---------------

(: bytes->dpacket (Bytes -> DataPacket))
(define (bytes->dpacket b)
  (cond [(lacks-full-header? b) (raise-parse-error "Invalid data packet")]
        [else (DataPacket (timestamp b) (destid b) (seqno b) (msgno b)
                          (Nat->Posn (take32 b 4)) (bitwise-bit-set? (take32 b 4) 29)
                          (subbytes b 16))]))

(: msgno (Bytes -> Natural))
(define (msgno b) (natcheck (bitoff 31 (bitoff 30 (bitoff 29 (take32 b 4))))))

(: seqno (Bytes -> Natural))
(define (seqno b) (natcheck (bitoff 31 (take32 b 0))))

(: Nat->Posn (Natural -> Posn))
(define (Nat->Posn n)
  (match (bitwise-bit-field n 30 32)
    [2 'First] ; (1 0)
    [0 'Middle] ; (0 0)
    [1 'Last] ; (0 1)
    [3 'Only])) ; (1 1)