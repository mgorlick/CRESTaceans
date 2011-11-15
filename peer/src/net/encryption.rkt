#lang racket/base

(require racket/contract
         racket/unit
         racket/runtime-path)
(provide (all-defined-out))

;; encryption for messages as used by tcp-peer.

;; foundational types.
(define nonce/c bytes?)
(define cipher/c bytes?)
(define message/c bytes?)
(define public-key/c bytes?)
;; function types.
(define encrypter/c (message/c . -> . (values cipher/c nonce/c)))
(define decrypter/c (cipher/c nonce/c . -> . message/c))
(define dencrypter-factory/c (public-key/c . -> . (values encrypter/c decrypter/c)))
(define pk-factory/c (-> (values public-key/c dencrypter-factory/c)))

(define/contract (do-DH-exchange my-PK i o)
  (public-key/c input-port? output-port? . -> . public-key/c)
  (write my-PK o)
  (flush-output o)
  (read i))

(define-signature encryption-unit^
  ((contracted
    ;; makes a local public key a function that
    ;; produces encrypter and decrypter functions when
    ;; supplied with a remote public key.
    
    ;; how the encrypt and decrypt process works:
    ;; 1. ask for a public key P_l and a function that, when given the remote public key P_r, returns an encrypter and decrypter.
    ;; 2. get P_r with `do-DH-exchange', supplying P_l
    ;; 3. call compute-secret* with P_r, receiving an encrypter and decrypter
    ;; 4. now this peer is free to encrypt and decrypt all the messages to the remote peer who holds P_r
    [make-pk/encrypter/decrypter pk-factory/c])))

;;; --------- crypto implementations ---------

;; a Unit that provides no encryption at all.
(define-unit no-encryption@
  (import)
  (export encryption-unit^)
  
  (define NOOP-BSTR #"")
  
  (define/contract (noop-encrypt m) encrypter/c (values m NOOP-BSTR)) ; just give the message back
  (define/contract (noop-decrypt m nonce) decrypter/c m) ; message is already decoded
  
  (define (make-pk/encrypter/decrypter)
    (values NOOP-BSTR 
            (λ (their-PK) ; don't care about it
              (values noop-encrypt noop-decrypt)))))

;; a Unit that provides NaCl-based encryption.
(define-runtime-path path/to/nacl/lib
  "../../../bindings/nacl/libnacl/libnacl.rkt")
(define-runtime-path path/to/nacl/crypto-box-helper
  "../../../bindings/nacl/libnacl/crypto-box.rkt")
(define-unit nacl-encryption@
  (import)
  (export encryption-unit^)
  
  (define crypto-box-keypair
    (dynamic-require path/to/nacl/lib 'crypto-box-keypair))
  (define encrypt-with-secret
    (dynamic-require path/to/nacl/crypto-box-helper 'encrypt-with-secret))
  (define decrypt-with-secret
    (dynamic-require path/to/nacl/crypto-box-helper 'decrypt-with-secret))
  (define compute-secret
    (dynamic-require path/to/nacl/crypto-box-helper 'compute-secret))
  
  (define (make-pk/encrypter/decrypter)
    (define-values (my-pk my-sk _) (crypto-box-keypair))
    
    (define/contract (compute-secret* their-pk)
      (public-key/c . -> . (values encrypter/c decrypter/c))
      (define/contract secret bytes? (compute-secret their-pk my-sk))
      
      (define/contract (encrypt message)
        encrypter/c
        (encrypt-with-secret message secret)) 
      
      (define/contract (decrypt cipher nonce)
        decrypter/c
        (decrypt-with-secret cipher nonce secret))
      (values encrypt decrypt))
    
    (values my-pk compute-secret*)))

#|(define-values/invoke-unit/infer no-encryption@)
;(define-values/invoke-unit/infer nacl-encryption@)
(define (test-encryption)
  (define-values (pA fkA) (make-pk/encrypter/decrypter))
  (define-values (pB fkB) (make-pk/encrypter/decrypter))
  (define-values (eA dA) (fkA pB))
  (define-values (eB dB) (fkB pA))
  (define message #"Hello world")
  (define-values (cipher1 nonce1) (eA message))
  (define-values (cipher2 nonce2) (eB message))
  (and (bytes=? message (dB cipher1 nonce1))
       (bytes=? message (dA cipher2 nonce2))))|#