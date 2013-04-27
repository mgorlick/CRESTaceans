#lang racket/base

(require
 ffi/unsafe
 (only-in "sodium.rkt" sodium define-sodium-function))

(provide crypto/sign/keys crypto/sign crypto/unsign)

;; Digital signatures.

(define CRYPTO-SIGN/BYTES            64) ; Number of bytes in a signature.
(define CRYPTO-SIGN/PUBLIC-KEY-BYTES 32) ; Number of bytes in verification key.
(define CRYPTO-SIGN/SECRET-KEY-BYTES 64) ; Number of bytes in signing key.

(define-cstruct _junk-ullong ((_ _ullong)))
(define JUNK-ULLONG-POINTER (make-junk-ullong 0))

;; Generate a verification/signing key pair.
; int crypto_sign_keypair(unsigned char *vk, unsigned char* sk)
(define-sodium-function crypto-sign-key-pair "crypto_sign_keypair" (_fun _bytes _bytes -> _int))

;; Generate a verification/signing key pair returning them as (values vk sk).
(define (crypto/sign/keys)
  (let ((vk (make-bytes CRYPTO-SIGN/PUBLIC-KEY-BYTES 0))
        (sk (make-bytes CRYPTO-SIGN/SECRET-KEY-BYTES 0)))
    (if (zero? (crypto-sign-key-pair vk sk))
        (values vk sk)
        (error 'crypto/sign/keys "unable to generate a verification/signing key pair"))))

;; This is a special purpose wrapper around the libsodium crypto_sign to help prevent Racket from crashing.
;; int racket_crypto_sign(unsigned char*       s,        ; Signed message (output).
;;                        unsigned long long*  s_length  ; Signed message length in bytes.
;;                        const unsigned char* m,        ; Message.
;;                        unsigned long long   m_length, ; Length of message in bytes.
;;                        const unsigned char* sk)       ; Signing key.
(define-sodium-function crypto-sign "crypto_sign" (_fun _bytes _junk-ullong-pointer _bytes _ullong _bytes -> _int))


;; Signs message m using signing key sk.
;; Returns a signed message where the first CRYPTO-SIGN/BYTES are the signature and the rest are message m.
(define (crypto/sign m sk)
  (when (not (= (bytes-length sk) CRYPTO-SIGN/SECRET-KEY-BYTES))
    (error 'crypto/sign "incorrect signing key length"))
  (let* ((m/length (bytes-length m))
         (signature (make-bytes (+ m/length CRYPTO-SIGN/BYTES) 0)))
    (crypto-sign signature JUNK-ULLONG-POINTER m m/length sk)
    signature))

;; int crypto_sign_open(unsigned char*        m,        ; Message (output).
;;                      unsigned long long*   m_length  ; Message length in bytes.
;;                      const unsigned char*  s,        ; Signed message.
;;                      unsigned long long    s_length, ; Length of signed message in bytes
;;                      const unsigned char*  vk)       ; Verification key.
(define-sodium-function crypto-sign-open "crypto_sign_open" (_fun _bytes _junk-ullong-pointer _bytes _ullong _bytes -> _int))

;; Given signed material signing and verification key vk return the original material if the signature is correct
;; and false otherwise.
(define (crypto/unsign signing vk)
  (when (not (= (bytes-length vk) CRYPTO-SIGN/PUBLIC-KEY-BYTES))
    (error 'crypto/unsign "incorrect verification key length"))
  (let ((signing/length (bytes-length signing)))
    (when (< signing/length CRYPTO-SIGN/BYTES)
      (error 'crypto/unsign "signed material is too short"))

    (let ((message (make-bytes (- signing/length CRYPTO-SIGN/BYTES) 0)))
      (if (zero? (crypto-sign-open message JUNK-ULLONG-POINTER signing signing/length vk))
          message
          #f))))