#lang racket

(require "msg.rkt")

(define/contract (add1/wrap n top)
  (exact-nonnegative-integer? exact-nonnegative-integer? . -> . exact-nonnegative-integer?)
  (define n++ (add1 n))
  (if (> n++ top) 0 n++))
(define payload (make-bytes 10000 65))

(define-values (i o) (tcp-connect "localhost" 5000))

(write-rec-byteslist o (seq-header 5 top32 top31))

(write-rec-byteslist o (msg-header 1 0 #t 25 (bytes-length payload)))
(write-rec-byteslist o payload)
(write-rec-byteslist o TRAILER)

(write-rec-byteslist o (ans-header 0 top31 #t top32 (bytes-length payload) 0))
(write-rec-byteslist o payload)
(write-rec-byteslist o TRAILER)

(write-rec-byteslist o (nul-header 0 0 0))
(write-rec-byteslist o TRAILER)

(write-rec-byteslist o (rpy-header 5 33 #f 120 (bytes-length payload)))
(write-rec-byteslist o payload)
(write-rec-byteslist o TRAILER)