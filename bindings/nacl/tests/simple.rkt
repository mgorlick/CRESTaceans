#lang racket/base

(require "../libnacl/libnacl.rkt")
(require "../libnacl/crypto-box.rkt")

;Get public/private key pairs for A and B
(define-values (a-pk a-sk)
  (let-values ([(pk sk r) (crypto-box-keypair)])
    (values pk sk)))

(define-values (b-pk b-sk)
  (let-values ([(pk sk r) (crypto-box-keypair)])
    (values pk sk)))

(define (test message)
  (let-values ([(cipher nonce) (encrypt-message message b-pk a-sk)])
    (printf "Original message: ~a~n" message)
    (printf "Decrypted message: ~a~n" (decrypt-cipher cipher nonce a-pk b-sk))))