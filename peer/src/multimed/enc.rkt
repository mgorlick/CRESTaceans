#! /usr/bin/env racket
#lang racket

(require "../../../bindings/vorbis/libvorbis.rkt")

(define outbound-host "127.0.0.1")
(define outbound-port 5000)
(define inbound-port 4999)

(define udp-read
  (let ([socket (let ([s (udp-open-socket)]) (udp-bind! s #f inbound-port) s)]
        [buffer (make-bytes 1000000)])
    (λ (encoder-thread)
      (let-values ([(len addr port) (udp-receive! socket buffer)])
        (thread-send encoder-thread (subbytes buffer 0 len))
        (udp-read encoder-thread)))))

(define udp-write
  (let ([socket (let ([s (udp-open-socket)]) (udp-connect! s outbound-host outbound-port) s)])
    (λ ()
      (let ([buffer (thread-receive)])
        (udp-send socket buffer)
        (udp-write)))))

(define (make-write-callback writer-thread)
  (λ (packet type)
    (thread-send writer-thread (ogg-packet-data packet))
    #t))

(define encoder (current-thread))
(define udp-reader (thread (λ () (udp-read encoder))))
(define udp-writer (thread (λ () (udp-write))))

(define output-packet (make-write-callback udp-writer))

(define enc (vorbisenc-new))
(vorbisenc-init enc output-packet)
(let loop ()
  (let ([buffer (thread-receive)])
    (vorbisenc-encode-pcm-samples enc buffer output-packet))
  (loop))