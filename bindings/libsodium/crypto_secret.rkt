#lang racket/base

(require
 ffi/unsafe
 (only-in "sodium.rkt" sodium define-sodium-function define-sodium-constant)
 (only-in "random.rkt" random/bytes/n))

(provide
 crypto/secret/key-bytes
 crypto/secret/nonce-bytes
 crypto/secret/primitive
 crypto/secret/key crypto/secret/box crypto/secret/unbox)


;; Authenticated symmetric encryption.

;; Pull the crypto/secret constants out of libsodium.
(define-sodium-constant crypto/secret/key-bytes         "crypto_secretbox_keybytes"     _int)
(define-sodium-constant crypto/secret/nonce-bytes       "crypto_secretbox_noncebytes"   _int)
(define-sodium-constant crypto/secret/plain-zero-bytes  "crypto_secretbox_zerobytes"    _int)
(define-sodium-constant crypto/secret/cipher-zero-bytes "crypto_secretbox_boxzerobytes" _int)
(define-sodium-constant crypto/secret/primitive         "crypto_secretbox_primitive"    _bytes)

(define CRYPTO-SECRET-BOX/KEY-BYTES      32) ; Length of symmetric key in bytes.
(define CRYPTO-SECRET-BOX/NONCE-BYTES    24) ; Length of nonces in bytes.
(define CRYPTO-SECRET-BOX/ZERO-BYTES     32) ; Number of leading 0 bytes in a message.
(define CRYPTO-SECRET-BOX/BOX-ZERO-BYTES 16) ; Number of leading 0 bytes in a cipertext.


(define (crypto/secret/key) (random/bytes/n CRYPTO-SECRET-BOX/KEY-BYTES))

;; int crypto_secretbox(unsigned char*       c,        // Ciphertext (output).
;;                      const unsigned char* m,        // Plaintext.
;;                      unsigned long long   m_length, // Length of plaintext in bytes.
;;                      const unsigned char* n,        // Nonce for ciphertext.
;;                      const unsigned char* k)        // Symmetric encryption key.
(define-sodium-function crypto-secret-box "crypto_secretbox" (_fun _bytes _bytes _ullong _bytes _bytes -> _int))

(define (crypto/secret/box m n k)
  ; Input sanity.
  (when (not (= (bytes-length k) CRYPTO-SECRET-BOX/KEY-BYTES))
    (error 'crypto/secret/box "incorrect secret key length"))
  (when (not (= (bytes-length n) CRYPTO-SECRET-BOX/NONCE-BYTES))
    (error 'crypto/secret/box "incorrect nonce length"))

  (let* ((m/pad/length (+ (bytes-length m) CRYPTO-SECRET-BOX/ZERO-BYTES))
         (m/pad (make-bytes m/pad/length 0))
         (c     (make-bytes m/pad/length 0)))
    (bytes-copy! m/pad CRYPTO-SECRET-BOX/ZERO-BYTES m)
    (crypto-secret-box c m/pad m/pad/length n k)
    (subbytes c CRYPTO-SECRET-BOX/BOX-ZERO-BYTES)))
    
;; int crypto_secretbox_open(unsigned char*       m,        // Plaintext (output).
;;                           const unsigned char* c,        // Ciphertext.
;;                           unsigned long long   c_length, // Length of ciphertext in bytes.
;;                           const unsigned char* n,        // Nonce.
;;                           const unsigned char* k)        // Symmetric encryption key.
(define-sodium-function crypto-secret-unbox "crypto_secretbox_open" (_fun _bytes _bytes _ullong _bytes _bytes -> _int))

(define (crypto/secret/unbox c n k)
  ; Input sanity.
  (when (not (= (bytes-length k) CRYPTO-SECRET-BOX/KEY-BYTES))
    (error 'crypto/secret/unbox "incorrect secret key length"))
  (when (not (= (bytes-length n) CRYPTO-SECRET-BOX/NONCE-BYTES))
    (error 'crypto/secret/unbox "incorrect nonce length"))

  (let ((c/length (bytes-length c)))
    (when (< c/length CRYPTO-SECRET-BOX/ZERO-BYTES)
      (error 'crypto/secret/unbox "ciphertext too short"))
    (let* ((c/pad/length (+ c/length CRYPTO-SECRET-BOX/BOX-ZERO-BYTES))
           (c/pad (make-bytes c/pad/length 0))
           (m/pad (make-bytes c/pad/length 0)))
      (bytes-copy! c/pad CRYPTO-SECRET-BOX/BOX-ZERO-BYTES c)
      (crypto-secret-unbox m/pad c/pad c/pad/length n k)
      (subbytes m/pad CRYPTO-SECRET-BOX/ZERO-BYTES))))