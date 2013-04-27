#lang racket/base

(require 
 (only-in "random.rkt" random/bytes/16 random/bytes/24 random/bytes/32 random/bytes/n)
 (only-in "crypto_box.rkt" crypto/box/nonce-bytes))

(provide crypto/nonce/random crypto/nonce/define)

(define NONCE-BYTES (crypto/box/nonce-bytes))

;; Helper functions for generating nonces.

;; Generate a random nonce. Define the function by querying libsodium on the expected nonce size.
(define crypto/nonce/random
  (case NONCE-BYTES
    ((16) random/bytes/16)
    ((24) random/bytes/24)
    ((32) random/bytes/32)
    (else (lambda () (random/bytes/n NONCE-BYTES)))))

;; Generate a nonce with unsigned integer value i.
(define (crypto/nonce/define i)
  (let ((n (make-bytes NONCE-BYTES 0)))
    (integer->integer-bytes
     i 8   ; Convert natural number i to 8 bytes.
     #f    ; Unsigned.
     #t    ; Big-endian.
     n (- NONCE-BYTES 8)) ; Store into 8 rightmost nonce bytes.
    n))
  
        