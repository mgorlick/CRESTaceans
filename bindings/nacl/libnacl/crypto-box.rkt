#lang racket/base

(require "libnacl.rkt")
(provide (all-defined-out))

;Function to generate a random byte sequence of length n. Use to generate nonces.
(define (random-bytes n)
  (define b# (make-bytes n))
  (for ([i (in-range n)])
    (bytes-set! b# i (random 256)))
  b#)

;Wrapper function to encrypt messages (byte strings)
;Automatically generates random nonce and returns it alongside the ciphertext
(define (encrypt-message m receivers-pk senders-sk)
  (let ([nonce (random-bytes crypto-box-NONCEBYTES)])
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

#|
The functions below use the precomputation interface to optimize by computing the shared
secret between two parties before any message exchange takes place. This only needs to be done
once regardless of the number of messages sent.

I'm thinking in practice we'll need some scheme to map identities to shared secrets so that
any time we are encrypting/decrypting a message to/from someone else, we can look up the
shared secret corresponding to that party. Maybe a hashtable or something?
|#

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