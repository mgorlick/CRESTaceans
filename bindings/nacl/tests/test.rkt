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
  (let ([padded-message (bytes-append (make-bytes crypto-box-ZEROBYTES) m)]
        [nonce (random-bytes crypto-box-NONCEBYTES)])
    (printf "~a ~n" padded-message)
    (let-values ([(cipher r)
                  (crypto-box padded-message nonce receivers-pk senders-sk)])
      (printf "~a ~n" (bytes-length cipher))
      (if (not (= 0 r))
          (error "Something is horribly wrong! No error codes in crypto box!")
          (values cipher nonce)))))

(define (encrypt-test m n r-pk s-sk)
   (let ([padded-message (bytes-append (make-bytes crypto-box-ZEROBYTES) m)])
     (crypto-box padded-message n r-pk s-sk)))

(define (decrypt-cipher c nonce senders-pk receivers-sk)
  (let-values ([(message r)
                (crypto-box-open c nonce senders-pk receivers-sk)])
    (if (= -1 r)
        (printf "Ciphertext verification failed. Message contents: ~a ~n" (bytes->string/latin-1 message))
        message)))

(define (test message)
  (let-values ([(cipher nonce) (encrypt-message message b-pk a-sk)])
    (printf "~a~n" (bytes-length nonce))
    (decrypt-cipher cipher nonce a-pk b-sk)))

(define mynonce (random-bytes crypto-box-NONCEBYTES))
(encrypt-test #"Hello" mynonce b-pk a-sk)
(encrypt-test #"Hello" mynonce b-pk a-sk)
(encrypt-test #"Hello" mynonce b-pk a-sk)
(let-values ([(c r) (encrypt-test #"Hello" mynonce b-pk a-sk)])
  (decrypt-cipher c mynonce a-pk b-sk))





