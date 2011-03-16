#! /usr/bin/env racket
#lang racket

(require "../../../bindings/vorbis/libvorbis.rkt"
         "util.rkt")

;; constants
(define outbound-host "127.0.0.1")
(define outbound-port 5000)
(define inbound-port 4999)
(define float-conversion-type 'naive) ;; handles gstreamer's peculiarity wrt network float serialization

;; component setup
(define (make-udp-reader inbound-port receiver)
  (λ-loop ([socket (let ([s (udp-open-socket)]) (udp-bind! s #f inbound-port) s)]
           [buffer (make-bytes 1000000)])
    (let-values ([(len addr port) (udp-receive! socket buffer)])
      (thread-send receiver (subbytes buffer 0 len)))))

(define (make-udp-writer outbound-host outbound-port)
  (λ-loop ([socket (let ([s (udp-open-socket)]) (udp-connect! s outbound-host outbound-port) s)])
    (let ([buffer (thread-receive)])
      (udp-send socket buffer))))

(define (make-encoder float-conversion-type receiver)
  (let ([enc (vorbisenc-new)]
        [output-packet (make-packet-out-callback receiver)])
    (vorbisenc-init enc output-packet)
    (λ-loop
        (let ([buffer (thread-receive)])
          (vorbisenc-encode-pcm-samples enc buffer float-conversion-type output-packet)))))

;; encoder stuff
(define (make-packet-out-callback receiver)
  (λ (packet type)
    (thread-send receiver (ogg-packet-data packet))
    #t))

;; a pipeline for processing PCM:
;; read from UDP in -> encode into vorbis -> write to UDP out
(define udp-writer (thread (make-udp-writer outbound-host outbound-port)))
(define encoder (thread (make-encoder float-conversion-type udp-writer)))
(define udp-reader (thread (make-udp-reader inbound-port encoder)))