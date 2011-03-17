#lang racket

(require "util.rkt")
(provide make-udp-reader
         make-udp-writer)

(define (make-udp-reader inbound-host inbound-port receiver)
  (λ-loop ([socket (let ([s (udp-open-socket)]) (udp-bind! s inbound-host inbound-port) s)]
           [buffer (make-bytes 1000000)])
    (let-values ([(len addr port) (udp-receive! socket buffer)])
      (thread-send receiver (subbytes buffer 0 len)))))

(define (make-udp-writer outbound-host outbound-port)
  (λ-loop ([socket (let ([s (udp-open-socket)]) (udp-connect! s outbound-host outbound-port) s)])
    (let ([buffer (thread-receive)])
      (udp-send socket buffer))))