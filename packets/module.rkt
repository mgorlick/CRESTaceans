#lang typed/racket

(require "packets.rkt"
         "data.rkt"
         "control.rkt"
         "send-data.rkt"
         "util.rkt")

(provide (all-defined-out)
         (all-from-out "packets.rkt"
                       "data.rkt"
                       "control.rkt"
                       "send-data.rkt"
                       "util.rkt"))

(: bytes->packet (Bytes Natural -> (U DataPacket ControlPacket)))
(define (bytes->packet buffer amt)
  (if (bitwise-bit-set? (integer-bytes->integer buffer #f #t 0 2) 15)
      (bytes->cpacket buffer amt)
      (bytes->dpacket buffer amt)))

(: packet->bytes ((U DataPacket ControlPacket) Bytes -> Natural))
(define (packet->bytes p buffer)
  (cond [(DataPacket? p) (dpacket->bytes p buffer)]
        [(ControlPacket? p) (cpacket->bytes p buffer)]))