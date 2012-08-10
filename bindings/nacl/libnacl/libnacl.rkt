#lang racket/base

(require ffi/unsafe)
(define lib (ffi-lib "libnacl"))

(provide 
 (prefix-out nacl: 
             (rename-out [crypto-box-keypair-wrap crypto-box-keypair]
                         [crypto-box-beforenm-wrap crypto-box-beforenm]
                         [crypto-box-afternm-wrap crypto-box-afternm]
                         [crypto-box-open-afternm-wrap crypto-box-open-afternm]
                         [crypto-sign-keypair-wrap crypto-sign-keypair]
                         [crypto-sign-wrap crypto-sign]
                         [crypto-hash-wrap crypto-hash])))

;;; Bindings to C NaCl wrapper

(define-syntax-rule (defnacl+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces (symbol->string 'obj) '((#rx"-" "_"))) lib typ)))
(define-syntax-rule (defnacl obj typ)
  (defnacl+ obj obj typ))
(define-syntax-rule (defnacl* typ obj ...)
  (begin (defnacl obj typ) ...))

; constant grabbers
(define-syntax-rule (define-constants-via-grabbers/provide (constant-name grabber-name) ...)
  (begin (defnacl* (_fun -> _int) grabber-name ...)
         (define constant-name (grabber-name)) ...
         (provide (prefix-out nacl: constant-name)) ...))

(define-constants-via-grabbers/provide
  [crypto-box-PUBLICKEYBYTES crypto-box-get-publickeybytes]
  [crypto-box-SECRETKEYBYTES crypto-box-get-secretkeybytes]
  [crypto-box-NONCEBYTES crypto-box-get-noncebytes]
  [crypto-box-BEFORENMBYTES crypto-box-get-beforenmbytes]
  [crypto-box-ZEROBYTES crypto-box-get-zerobytes]
  [crypto-box-BOXZEROBYTES crypto-box-get-boxzerobytes]
  [crypto-sign-PUBLICKEYBYTES crypto-sign-get-publickeybytes]
  [crypto-sign-SECRETKEYBYTES crypto-sign-get-secretkeybytes]
  [crypto-sign-BYTES crypto-sign-get-bytes]
  [crypto-hash-BYTES crypto-hash-get-bytes])

;;; AUTHENTICATED ENCRYPTION

;Function to generate a random byte sequence of length n. Use to generate nonces.
(define (make-nonce)
  (define b# (make-bytes crypto-box-NONCEBYTES))
  (for ([i (in-range crypto-box-NONCEBYTES)])
    (bytes-set! b# i (random 256)))
  b#)

(define zeroes (make-bytes crypto-box-ZEROBYTES))
(define boxzeroes (make-bytes crypto-box-BOXZEROBYTES))

(defnacl crypto-box-keypair-wrap 
  (_fun (pk : (_bytes o crypto-box-PUBLICKEYBYTES))
        (sk : (_bytes o crypto-box-SECRETKEYBYTES))
        -> (r : _int) 
        -> (if (zero? r)
               (values pk sk)
               (error "Unable to generate a key pair"))))

(defnacl crypto-box-wrap 
  (_fun (m n pk sk) ::
        (ciphertext : (_bytes o message-length))
        (message : _bytes = (bytes-append zeroes m))
        (message-length : _ullong = (bytes-length message))
        (n : _bytes) (pk : _bytes) (sk : _bytes)
        -> (r : _int)
        -> (if (zero? r)
               (subbytes ciphertext crypto-box-BOXZEROBYTES)
               (error "Unable to box message"))))

(defnacl crypto-box-open-wrap 
  (_fun (c n pk sk) :: 
        (message : (_bytes o cipher-length))
        (ciphertext : _bytes = (bytes-append boxzeroes c))
        (cipher-length : _ullong = (bytes-length ciphertext))
        (n : _bytes) (pk : _bytes) (sk : _bytes)
        -> (r : _int)
        -> (if (= -1 r)
               (error "Ciphertext verification failed. Message contents: " (bytes->string/latin-1 message))
               (subbytes message crypto-box-ZEROBYTES))))

(defnacl crypto-box-beforenm-wrap
  (_fun (secret : (_bytes o crypto-box-BEFORENMBYTES))
        (pk : _bytes) (sk : _bytes)
        -> (r : _int)
        -> (if (eq? 0 r)
               secret
               (error "Error in computing shared secret"))))

(defnacl crypto-box-afternm-wrap 
  (_fun (msg shared-secret) ::
        (ciphertext : (_bytes o message-length))
        (message : _bytes = (bytes-append zeroes msg)) 
        (message-length : _ullong = (bytes-length message))
        (nonce : _bytes = (make-nonce))
        (shared-secret : _bytes)
        -> (r : _int)
        -> (if (zero? r)
               (values ciphertext nonce)
               (error "Error in boxing message"))))

(defnacl crypto-box-open-afternm-wrap
  (_fun (ciphertext nonce shared-secret) ::
        (message : (_bytes o cipher-length))
        (ciphertext : _bytes)
        (cipher-length : _ullong = (bytes-length ciphertext))
        (nonce : _bytes)
        (shared-secret : _bytes)
        -> (r : _int)
        -> (if (eq? 0 r)
               (subbytes message crypto-box-ZEROBYTES)
               (error "Ciphertext verification failed. Message contents: " 
                      (bytes->string/latin-1 message)))))

;;; SIGNATURES

(defnacl crypto-sign-keypair-wrap
  (_fun (pk : (_bytes o crypto-sign-PUBLICKEYBYTES))
        (sk : (_bytes o crypto-sign-SECRETKEYBYTES))
        -> (r : _int)
        -> (if (zero? r) (values pk sk) (error "failed to allocate signing key pair"))))

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

;;; HASH

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