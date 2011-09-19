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

;Compute shared secret from each perspective
(define-values (a-k b-k)
  (values (compute-secret b-pk a-sk) (compute-secret a-pk b-sk)))

(define (test message)
  (let-values ([(cipher nonce) (encrypt-with-secret message a-k)])
    (printf "Original message: ~a~n" message)
    (printf "Decrypted message: ~a~n" (decrypt-with-secret cipher nonce b-k))))