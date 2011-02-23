#lang racket

(require "../libnacl/libnacl.rkt")

;Function to generate a random byte sequence of length n. Use to generate nonces.
(define (random-bytes n)
  (define (make-random bytes index)
    (if (>= index (bytes-length bytes))
        bytes
        (begin
          (bytes-set! bytes index (random 256))
          (make-random bytes (add1 index)))))
  (make-random (make-bytes n) 0))

;Get public/private key pairs for A and B
(define-values (a-pk a-sk)
  (let-values ([(pk sk r) (crypto-box-keypair)])
    (values pk sk)))

(define-values (b-pk b-sk)
  (let-values ([(pk sk r) (crypto-box-keypair)])
    (values pk sk)))

(define (encrypt-message m receivers-pk senders-sk)
  (let ([nonce (random-bytes crypto-box-NONCEBYTES)])
    (let-values ([(cipher r)
                  (crypto-box m nonce receivers-pk senders-sk)])
      (if (not (= 0 r))
          (error "Something is horribly wrong! No error codes in crypto box!")
          (values cipher nonce)))))

(define (decrypt-cipher c nonce senders-pk receivers-sk)
  (let-values ([(message r)
                (crypto-box-open c nonce senders-pk receivers-sk)])
    (if (= -1 r)
        (error "Ciphertext verification failed. Message contents: " (bytes->string/latin-1 message))
        message)))

(define (test message)
  (let-values ([(cipher nonce) (encrypt-message message b-pk a-sk)])
    (printf "Original message: ~a~n" message)
    (printf "Decrypted message: ~a~n" (decrypt-cipher cipher nonce a-pk b-sk))))





