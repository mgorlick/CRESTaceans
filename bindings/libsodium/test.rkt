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


;; A snippet of the Gettysburg Address (Abraham Lincoln, November 19, 1863) for test plaintext.
(define PLAINTEXT
  #"Four score and seven years ago our fathers brought forth on this continent a new nation, conceived in liberty, \
and dedicated to the proposition that all men are created equal. \
Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. \
We are met on a great battle field of that war. We have come to dedicate a portion of that field, \
as a final resting place for those who here gave their lives that that nation might live. \
It is altogether fitting and proper that we should do this.")


(define (test/crypto/box)
  (let-values ([(pk/sender sk/sender)     (crypto/box/keys)]
               [(pk/receiver sk/receiver) (crypto/box/keys)])
    (let ((nonce (crypto/nonce/random)))
      (crypto/box PLAINTEXT nonce pk/receiver sk/sender))))
    
(define (test/crypto/unbox)
  (let-values ([(pk/sender sk/sender)     (crypto/box/keys)]
               [(pk/receiver sk/receiver) (crypto/box/keys)])
    (let* ((nonce (crypto/nonce/random))
           (ciphertext (crypto/box PLAINTEXT nonce pk/receiver sk/sender)))
      (crypto/unbox ciphertext nonce pk/sender sk/receiver))))

(define (test/crypto/box/precompute)
  (let-values ([(pk/sender sk/sender) (crypto/box/keys)]
               [(pk/receiver sk/receiver) (crypto/box/keys)])
    (crypto/box/precompute pk/receiver sk/sender)))

(define (test/crypto/box/pre)
    (let-values ([(pk/a sk/a)     (crypto/box/keys)]
                 [(pk/b sk/b) (crypto/box/keys)])
      (let* ((pre/a (crypto/box/precompute pk/b sk/a)) ; For encrypting messages sent from a to b.
             (pre/b (crypto/box/precompute pk/a sk/b)) ; For decrypting messages sent to b from a.
             (nonce (crypto/nonce/random))
             (gold  (crypto/box PLAINTEXT nonce pk/b sk/a)) ; GOLD ciphertext for a message from a to b.
             (ciphertext (crypto/box/pre PLAINTEXT nonce pre/a)))
        (write gold)       (newline) (newline)
        (write ciphertext) (newline) (newline)
        (write (if (bytes=? gold ciphertext) "same ciphertext" "different ciphertext"))
        (newline) (newline)
        (write (crypto/unbox     ciphertext nonce pk/a sk/b))
        (newline) (newline)
        (write (crypto/unbox/pre ciphertext nonce pre/b))
        (newline))))

(define (test/crypto/sign)
  (let-values ([(vk sk) (crypto/sign/keys)])
    (let* ((signature (crypto/sign PLAINTEXT sk))
           (message   (crypto/unsign signature vk)))
      (write signature)
      (newline) (newline)
      (write message)
      (newline))))
 
(define (test/crypto/secret/box)
  (let* ((k (crypto/secret/key))
         (nonce (crypto/nonce/random))
         (ciphertext (crypto/secret/box PLAINTEXT nonce k)))
    (crypto/secret/unbox ciphertext nonce k)))

(define (test/crypto/authenticate)
  (let* ((m PLAINTEXT)
         (k (crypto/secret/key))
         (t (crypto/authenticate/token m k)))
    (write t)
    (newline) (newline)
    (write m)
    (newline) (newline)
    (crypto/authenticate? t m k)))

(define (test/crypto/hash)
  (crypto/hash PLAINTEXT))

(define (test/crypto/hash/short)
  (let ((k (crypto/hash/short/key)))
    (crypto/hash/short PLAINTEXT k)))
