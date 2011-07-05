#lang racket/base

(require racket/contract
         "../../../bindings/nacl/libnacl/libnacl.rkt"
         "../../../bindings/nacl/libnacl/crypto-box.rkt")
(provide make-pk/encrypter/decrypter
         do-DH-exchange
         encrypter/c
         decrypter/c)

;; encryption for messages as used by tcp-peer.

(define encrypter/c (bytes? bytes? . -> . (values bytes? bytes?)))
(define decrypter/c (bytes? bytes? bytes? . -> . bytes?))

(define/contract (make-pk/encrypter/decrypter)
  (-> (values bytes? encrypter/c decrypter/c))
  
  (define-values (my-pk my-sk _) (crypto-box-keypair))
  
  (define (compute-secret* their-pk)
    (compute-secret their-pk my-sk))
  
  (define/contract (encrypt their-pk message)
    (bytes? bytes? . -> . (values bytes? bytes?))
    (encrypt-with-secret message (compute-secret* their-pk)))
  
  (define/contract (decrypt their-pk cipher nonce)
    (bytes? bytes? bytes? . -> . bytes?)
    (decrypt-with-secret cipher nonce (compute-secret* their-pk)))
  
  (values my-pk encrypt decrypt))

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