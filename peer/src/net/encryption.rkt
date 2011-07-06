#lang racket/base

(require racket/contract
         "../../../bindings/nacl/libnacl/libnacl.rkt"
         "../../../bindings/nacl/libnacl/crypto-box.rkt")
(provide make-pk/encrypter/decrypter
         do-DH-exchange
         encrypter/c
         decrypter/c)

;; encryption for messages as used by tcp-peer.

(define encrypter/c (bytes? . -> . (values bytes? bytes?)))
(define decrypter/c (bytes? bytes? . -> . bytes?))
(define pk-setter/c (bytes? . -> . (values encrypter/c decrypter/c)))

;; how the encrypt and decrypt process works:
;; 1. ask for a public key P_l and a function that, when given the remote public key P_r, returns an encrypter and decrypter.
;; 2. get P_r with `do-DH-exchange', supplying P_l
;; 3. call compute-secret* with P_r, receiving an encrypter and decrypter
;; 4. now this peer is free to encrypt and decrypt all the messages to the remote peer who holds P_r
(define/contract (make-pk/encrypter/decrypter)
  (-> (values bytes? pk-setter/c))
  
  (define-values (my-pk my-sk _) (crypto-box-keypair))
  
  (define secret #f)
  
  (define/contract (encrypt message)
    (bytes? . -> . (values bytes? bytes?))
    (encrypt-with-secret message secret))
  
  (define/contract (decrypt cipher nonce)
    (bytes? bytes? . -> . bytes?)
    (decrypt-with-secret cipher nonce secret))
  
  (define/contract (compute-secret* their-pk)
    (bytes? . -> . (values encrypter/c decrypter/c))
    (set! secret (compute-secret their-pk my-sk))
    (values encrypt decrypt))
  
  (values my-pk compute-secret*))

(define/contract (do-DH-exchange my-PK i o)
  (bytes? input-port? output-port? . -> . bytes?)
  (write (vector my-PK) o)
  (flush-output o)
  (vector-ref (read i) 0))

(define (test-encryption)
  (define-values (pA eA dA) (make-pk/encrypter/decrypter))
  (define-values (pB eB dB) (make-pk/encrypter/decrypter))
  (define message #"Hello world")
  (define-values (cipher nonce) (eA pB message))
  (bytes=? message (dB pA cipher nonce)))