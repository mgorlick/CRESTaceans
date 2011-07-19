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
 (all-defined-out))


(define-syntax-rule (defnacl+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) lib typ)))
(define-syntax-rule (defnacl obj typ)
  (defnacl+ obj obj typ))
(define-syntax-rule (defnacl* typ obj ...)
  (begin (defnacl obj typ) ...))

(defnacl* (_fun -> _int)
  crypto-box-get-publickeybytes
  crypto-box-get-secretkeybytes
  crypto-box-get-noncebytes
  crypto-box-get-beforenmbytes
  crypto-box-get-zerobytes
  crypto-box-get-boxzerobytes)

(define crypto-box-PUBLICKEYBYTES (crypto-box-get-publickeybytes))
(define crypto-box-SECRETKEYBYTES (crypto-box-get-secretkeybytes))
(define crypto-box-NONCEBYTES (crypto-box-get-noncebytes))
(define crypto-box-BEFORENMBYTES (crypto-box-get-beforenmbytes))
(define crypto-box-ZEROBYTES (crypto-box-get-zerobytes))
(define crypto-box-BOXZEROBYTES (crypto-box-get-boxzerobytes))

(define zeroes (make-bytes crypto-box-ZEROBYTES))
(define boxzeroes (make-bytes crypto-box-BOXZEROBYTES))

(defnacl crypto-box-keypair-wrap
  (_fun (pk : (_bytes o crypto-box-PUBLICKEYBYTES))
        (sk : (_bytes o crypto-box-SECRETKEYBYTES))
        -> (r : _int)
        -> (values pk sk r)))

(defnacl crypto-box-wrap
  (_fun (m n pk sk) ::
        (ciphertext : (_bytes o message-length))
        (message : _bytes = (bytes-append zeroes m))
        (message-length : _ullong = (bytes-length message))
        (n : _bytes) (pk : _bytes) (sk : _bytes)
        -> (r : _int)
        -> (values 
            (cast (ptr-add ciphertext crypto-box-BOXZEROBYTES)
                  _pointer (_bytes o (- message-length crypto-box-BOXZEROBYTES)))
            r)))

(defnacl crypto-box-open-wrap
  (_fun (c n pk sk) :: 
        (message : (_bytes o cipher-length))
        (ciphertext : _bytes = (bytes-append boxzeroes c))
        (cipher-length : _ullong = (bytes-length ciphertext))
        (n : _bytes) (pk : _bytes) (sk : _bytes)
        -> (r : _int)
        -> (values 
            (cast (ptr-add message crypto-box-ZEROBYTES)
                  _pointer (_bytes o (- cipher-length crypto-box-ZEROBYTES)))
            r)))

(defnacl crypto-box-beforenm-wrap 
  (_fun (k : (_bytes o crypto-box-BEFORENMBYTES))
        (pk : _bytes) (sk : _bytes)
        -> (r : _int) -> (values k r)))

(defnacl crypto-box-afternm-wrap 
  (_fun (m n k) ::
        (ciphertext : (_bytes o message-length))
        (message : _bytes = (bytes-append zeroes m)) 
        (message-length : _ullong = (bytes-length message))
        (n : _bytes) (k : _bytes)
        -> (r : _int)
        -> (values 
            (cast (ptr-add ciphertext crypto-box-BOXZEROBYTES)
                  _pointer (_bytes o (- message-length crypto-box-BOXZEROBYTES)))
            r)))

(defnacl crypto-box-open-afternm-wrap
  (_fun (c n k) ::
        (message : (_bytes o cipher-length))
        (ciphertext : _bytes = (bytes-append boxzeroes c))
        (cipher-length : _ullong = (bytes-length ciphertext))
        (n : _bytes) (k : _bytes)
        -> (r : _int)
        -> (values 
            (cast (ptr-add message crypto-box-ZEROBYTES)
                  _pointer (_bytes o (- cipher-length crypto-box-ZEROBYTES)))
            r)))