#lang racket/base

(require
 ffi/unsafe
 (only-in "random.rkt" random/bytes/16 random/bytes/24 random/bytes/32 random/bytes/n)
 (only-in "sodium.rkt" sodium define-sodium-function define-sodium-constant))

(provide
 crypto/hash/short/hash-bytes
 crypto/hash/short/key-bytes
 crypto/hash/short/primitive
 crypto/hash/short/key crypto/hash/short)

(define-sodium-constant crypto/hash/short/hash-bytes "crypto_shorthash_bytes"     _int)
(define-sodium-constant crypto/hash/short/key-bytes  "crypto_shorthash_keybytes"  _int)
(define-sodium-constant crypto/hash/short/primitive  "crypto_shorthash_primitive" _bytes)

(define BYTES-PER-HASH (crypto/hash/short/hash-bytes))
(define BYTES-PER-KEY  (crypto/hash/short/key-bytes))

;; Generate a random short hash key. Define the function by querying libsodium on the expected key size.
(define crypto/hash/short/key
  (case BYTES-PER-KEY
    ((16) random/bytes/16)
    ((24) random/bytes/24)
    ((32) random/bytes/32)
    (else (lambda () (random/bytes/n BYTES-PER-KEY)))))


;; int crypto_shorthash(unsigned char *out,           // Hash (output).
;;                      const unsigned char *in,      // Source material (input).
;;                      unsigned long long in_length, // Length of source material in bytes.
;;                      const unsigned char *k)       // Secret key.
(define-sodium-function crypto-hash-short "crypto_shorthash" (_fun _bytes _bytes _ullong _bytes -> _int))


(define (crypto/hash/short m k)
  (when (not (= (bytes-length k) BYTES-PER-KEY))
    (error 'crypto/hash/short "incorrect key size"))
  (let ((hash (make-bytes BYTES-PER-HASH 0)))
    (crypto-hash-short hash m (bytes-length m) k)
    hash))