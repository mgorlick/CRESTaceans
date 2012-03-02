#lang racket/base

(require "libnacl.rkt")
(provide (all-defined-out))

;Wrapper function to encrypt messages (byte strings)
;Automatically generates random nonce and returns it alongside the ciphertext
(define (encrypt-message m receivers-pk senders-sk)
  (let ([nonce (make-nonce)])
    (let-values ([(cipher r)
                  (crypto-box m nonce receivers-pk senders-sk)])
      (if (not (= 0 r))
          (error "Something is horribly wrong! No error codes in crypto box!")
          (values cipher nonce)))))

;Wrapper function for decrypting ciphertexts
;Checks for error code before returning decrypted message
(define (decrypt-cipher c nonce senders-pk receivers-sk)
  (let-values ([(message r)
                (crypto-box-open c nonce senders-pk receivers-sk)])
    (if (= -1 r)
        (error "Ciphertext verification failed. Message contents: " (bytes->string/latin-1 message))
        message)))

;Computes a shared secret based on other party's public key and self's public key
(define (compute-secret pk sk)
  (let-values ([(secret r) (crypto-box-beforenm pk sk)])
    (if (eq? 0 r)
        secret
        (error "Error in computing shared secret"))))

;Encrypts message based on shared secret, returning ciphertext and nonce
(define (encrypt-with-secret m shared-k)
  (let ([nonce (random-bytes crypto-box-NONCEBYTES)])
    (let-values ([(cipher r) (crypto-box-afternm m nonce shared-k)])
      (if (eq? 0 r)
          (values cipher nonce)
          (error "Error in encryption")))))

;Decrypts message based on shared secret and nonce, returning decrypted message
(define (decrypt-with-secret c nonce shared-k)
  (let-values ([(message r) (crypto-box-open-afternm c nonce shared-k)])
    (if (eq? 0 r)
        message
        (error "Ciphertext verification failed. Message contents: " (bytes->string/latin-1 message)))))