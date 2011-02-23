#lang racket

(require "../libvorbis/libvorbis.rkt")

(define signed? #t)
(define big? #f)

(define (ogg-packet->bytes o)
  (bytes-append (ogg-packet-packet o)
                (integer->integer-bytes (ogg-packet-bytes o) longsize signed? big?)
                (integer->integer-bytes (ogg-packet-b-o-s o) longsize signed? big?)
                (integer->integer-bytes (ogg-packet-e-o-s o) longsize signed? big?)
                (integer->integer-bytes (ogg-packet-granulepos o) ogg64size signed? big?)
                (integer->integer-bytes (ogg-packet-packetno o) ogg64size signed? big?)))

(define my-packet (make-ogg-packet #"ABCDEFGHI" 37 0 0 500 1))
(define my-packet-bytes (ogg-packet->bytes my-packet))

(let ([socket (udp-open-socket)])
  (let loop ()
    (udp-send-to socket "localhost" 44000 my-packet-bytes)
    (loop)))