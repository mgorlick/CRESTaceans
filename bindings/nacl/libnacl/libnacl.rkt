#lang racket/base

(require ffi/unsafe)
(define lib (ffi-lib "libnacl"))

(provide
 (rename-out [crypto-box-keypair-wrap crypto-box-keypair]
             [crypto-box-wrap crypto-box]
             [crypto-box-open-wrap crypto-box-open]
             [crypto-box-beforenm-wrap crypto-box-beforenm]
             [crypto-box-afternm-wrap crypto-box-afternm]
             [crypto-box-open-afternm-wrap crypto-box-open-afternm])
 (rename-out [crypto-hash-wrap crypto-hash])
 (rename-out [crypto-sign-wrap crypto-sign]
             [crypto-sign-open-wrap crypto-sign-open]
             [crypto-sign-keypair-wrap crypto-sign-keypair])
 make-nonce)

(define-syntax-rule (defnacl+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) lib typ)))
(define-syntax-rule (defnacl obj typ)
  (defnacl+ obj obj typ))
(define-syntax-rule (defnacl* typ obj ...)
  (begin (defnacl obj typ) ...))

; constant grabbers

(defnacl* (_fun -> _int)
  crypto-box-get-publickeybytes
  crypto-box-get-secretkeybytes
  crypto-box-get-noncebytes
  crypto-box-get-beforenmbytes
  crypto-box-get-zerobytes
  crypto-box-get-boxzerobytes
  crypto-sign-get-publickeybytes
  crypto-sign-get-secretkeybytes
  crypto-sign-get-bytes
  crypto-hash-get-bytes)

;;; AUTHENTICATED ENCRYPTION

(define crypto-box-PUBLICKEYBYTES (crypto-box-get-publickeybytes))
(define crypto-box-SECRETKEYBYTES (crypto-box-get-secretkeybytes))
(define crypto-box-NONCEBYTES (crypto-box-get-noncebytes))
(define crypto-box-BEFORENMBYTES (crypto-box-get-beforenmbytes))
(define crypto-box-ZEROBYTES (crypto-box-get-zerobytes))
(define crypto-box-BOXZEROBYTES (crypto-box-get-boxzerobytes))

;Function to generate a random byte sequence of length n. Use to generate nonces.
(define (make-nonce)
  (define b# (make-bytes crypto-box-NONCEBYTES))
  (for ([i (in-range crypto-box-NONCEBYTES)])
    (bytes-set! b# i (random 256)))
  b#)

(define zeroes (make-bytes crypto-box-ZEROBYTES))
(define boxzeroes (make-bytes crypto-box-BOXZEROBYTES))

(defnacl crypto-box-keypair-wrap (_fun (pk : (_bytes o crypto-box-PUBLICKEYBYTES))
                                       (sk : (_bytes o crypto-box-SECRETKEYBYTES))
                                       -> (r : _int) -> (values pk sk r)))

(defnacl crypto-box-wrap (_fun (m n pk sk) ::
                               (ciphertext : (_bytes o message-length))
                               (message : _bytes = (bytes-append zeroes m))
                               (message-length : _ullong = (bytes-length message))
                               (n : _bytes) (pk : _bytes) (sk : _bytes)
                               -> (r : _int) -> (values (subbytes ciphertext crypto-box-BOXZEROBYTES) r)))

(defnacl crypto-box-open-wrap (_fun (c n pk sk) :: 
                                    (message : (_bytes o cipher-length))
                                    (ciphertext : _bytes = (bytes-append boxzeroes c))
                                    (cipher-length : _ullong = (bytes-length ciphertext))
                                    (n : _bytes) (pk : _bytes) (sk : _bytes)
                                    -> (r : _int) -> (values (subbytes message crypto-box-ZEROBYTES) r)))

(defnacl crypto-box-beforenm-wrap
  (_fun (k : (_bytes o crypto-box-BEFORENMBYTES))
        (pk : _bytes) (sk : _bytes)
        -> (r : _int)
        -> (values k r)))

(defnacl crypto-box-afternm-wrap 
  (_fun (m n k) ::
        (ciphertext : (_bytes o message-length))
        (message : _bytes = (bytes-append zeroes m)) 
        (message-length : _ullong = (bytes-length message))
        (n : _bytes) (k : _bytes)
        -> (r : _int)
        -> (values ciphertext r)))

(defnacl crypto-box-open-afternm-wrap
  (_fun (c n k) ::
        (message : (_bytes o cipher-length))
        (ciphertext : _bytes = c)
        (cipher-length : _ullong = (bytes-length ciphertext))
        (n : _bytes) (k : _bytes)
        -> (r : _int)
        -> (values
            (subbytes message crypto-box-ZEROBYTES)
            r)))

;;; SIGNATURES

(define crypto-sign-PUBLICKEYBYTES (crypto-sign-get-publickeybytes))
(define crypto-sign-SECRETKEYBYTES (crypto-sign-get-secretkeybytes))
(define crypto-sign-BYTES (crypto-sign-get-bytes))

(defnacl crypto-sign-keypair-wrap
  (_fun (pk : (_bytes o crypto-sign-PUBLICKEYBYTES))
        (sk : (_bytes o crypto-sign-SECRETKEYBYTES))
        -> (r : _int) -> (if (zero? r) (values pk sk) (error "failed to allocate signing key pair"))))

(defnacl crypto-sign-wrap
  (_fun (sk message) ::
        (signature : (_bytes o (+ (bytes-length message) crypto-sign-BYTES)))
        (signature-length : (_ptr o _ullong))
        (message : _bytes)
        (mlen : _ullong = (bytes-length message))
        (sk : _bytes)
        -> (r : _int)
        -> (if (zero? r)
               (subbytes signature 0 signature-length)
               (error "failed to sign"))))

(defnacl crypto-sign-open-wrap
  (_fun (pk signed-message) ::
        (message : (_bytes o (bytes-length signed-message)))
        (message-length : (_ptr o _ullong))
        (signed-message : _bytes)
        (signed-message-length : _ullong = (bytes-length signed-message))
        (pk : _bytes)
        -> (r : _int)
        -> (if (zero? r)
               (subbytes message 0 message-length)
               (error "failed to unsign/open"))))

#|(define-values (pk sk) (crypto-sign-keypair-wrap))
pk
sk
(crypto-sign-open-wrap pk (crypto-sign-wrap sk #"Hello world"))|#

;;; 

;;; HASH

(define crypto-hash-BYTES (crypto-hash-get-bytes))

(defnacl crypto-hash-wrap
  (_fun (message) ::
        (hash : (_bytes o crypto-hash-BYTES))
        (message : _bytes)
        (message-length : _ullong = (bytes-length message))
        -> (r : _int)
        -> (if (zero? r)
               hash
               (error "Failed to hash"))))

;(crypto-hash-wrap #"Hello world")