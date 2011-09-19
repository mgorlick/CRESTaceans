#lang racket/base

(require "../../../bindings/fastlz/fastlz.rkt")
(provide compress
         decompress)

;; compression for outgoing messages and decompression for incoming messages,
;; as used by tcp-peer.

(define (compress msg)
  (define o (open-output-bytes))
  (write msg o)
  (define b# (get-output-bytes o))
  (vector (compress-bytes b#) (bytes-length b#)))

(define (decompress msg)
  (read (open-input-bytes (uncompress-bytes (vector-ref msg 0) (vector-ref msg 1)))))