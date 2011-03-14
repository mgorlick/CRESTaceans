#! /usr/bin/env racket
#lang racket

(require "../../../bindings/vorbis/libvorbis.rkt")

(define outbound-host "127.0.0.1")
(define outbound-port 5000)
(define inbound-port 4999)

(define udp-read
  (let ([socket (let ([s (udp-open-socket)]) (udp-bind! s #f inbound-port) s)]
        [buffer (make-bytes 10000)])
    (λ (encoder-thread) 
      (let-values ([(len addr port) (udp-receive! socket buffer)])
        (thread-send encoder-thread (subbytes buffer 0 len))
        (udp-read encoder-thread)))))

(define write-socket (udp-open-socket))
(udp-connect! write-socket outbound-host outbound-port)

(define (udp-write)
  (let ([buffer (thread-receive)])
    (printf "sending packet of size ~a~n" (bytes-length buffer))
    (udp-send write-socket buffer)
    (udp-write)))

(define (make-write-callback writer-thread)
    (λ (packet)
      (thread-send writer-thread (ogg-packet-data packet))
      #t))

;(define udp-reader (thread (λ () (udp-read current-thread))))
(define udp-writer (thread (λ () (udp-write))))

(define output-packet (make-write-callback udp-writer))

(define enc (vorbisenc-new))
(define id (ogg-packet-new))
(define comment (ogg-packet-new))
(define codebook (ogg-packet-new))
(vorbisenc-init enc id comment codebook)
(output-packet id)
(output-packet comment)
(output-packet codebook)
;(vorbisenc-delete enc)