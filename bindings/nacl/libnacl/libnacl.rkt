#lang racket

(require ffi/unsafe)

(define libnacl (ffi-lib "libnacl"))

(define crypto-box
  (get-ffi-obj 'crypto_box_curve25519xsalsa20poly1305
               libnacl
               (_fun _string _string -> _void)))