#lang racket/base

(require
 "random.rkt"
 "crypto_authenticate.rkt"
 "crypto_box.rkt"
 "crypto_hash.rkt"
 "crypto_nonce.rkt"
 "crypto_secret.rkt"
 "crypto_short_hash.rkt"
 "crypto_sign.rkt")

;; Convenient one-stop shopping for all of the libsodium-based crypto/* functions plus
;; some convenient shorthand for generating random byte strings.

(provide
 (all-from-out
  "random.rkt"
  "crypto_authenticate.rkt"
  "crypto_box.rkt"
  "crypto_hash.rkt"
  "crypto_nonce.rkt"
  "crypto_secret.rkt"
  "crypto_short_hash.rkt"
  "crypto_sign.rkt"))

;; The code in the various crypto_*.rkt uses the following abbreviations for variables:
;; c  - ciphertext
;; m  - message or plaintext
;; n  - nonce
;; pk - public key
;; sk - matching secret key for a public key or a matching signing key for a public verification key
;; k  - precomputed combination of public and secret keys or a shared secret key
;; vk - public verification key for signatures
;; s  - signed material
;; t  - authentication token

















