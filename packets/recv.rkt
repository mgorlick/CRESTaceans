#lang typed/racket

(require "packets.rkt"
         "data.rkt"
         "control.rkt")

(provide (all-defined-out))

(: bytes->packet (Bytes -> (Option (U DataPacket ControlPacket))))
(define (bytes->packet b)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (if (bitwise-bit-set? (integer-bytes->integer b #f #t 0 2) 15)
        (bytes->cpacket b)
        (bytes->dpacket b))))

(: packet->bytes ((U DataPacket ControlPacket) -> Bytes))
(define (packet->bytes p)
  (cond [(DataPacket? p) (dpacket->bytes p)]
        [(ControlPacket? p) (cpacket->bytes p)]))