#lang racket

(require "net/base64-url.rkt"
         (planet vyzo/crypto))

;; CONSTRUCTION
(define-struct/contract 
  clan 
  ([thd thread?] ; reference to thread the clan is running in
   [shared-key-calculator (bytes? . -> . bytes?)]
   [decryptor (bytes? bytes? . -> . bytes?)]
   [encryptor (bytes? . -> . bytes?)]
   [mac-valid? (bytes? bytes? . -> . boolean?)]
   [pk bytes?]
   [pk-urlencoded (and/c bytes? base64-url-encoded?)]
   ))

; make-new-clan: -> clan
; create a clan with a randomly generated public and private key
; that is ready to calculate shared keys given another clan's key
; and validate a presented shared key
; precompute the encoded public shared key to avoid encoding it over and over again later
(define (make-new-clan)
  
  ; my-public-key is the key that will appear in the URLs that name members of this clan
  ; my-private-key should never leave the lexical scope of `make-new-clan'!
  (define-values (my-private-key my-public-key) (generate-key dh:1024))
  
  ; shared-key-calculator: bytestring -> bytestring
  ; a procedure that calculates a shared key with a
  ; given remote public key using the private key
  (define shared-key-calculator (curry compute-key my-private-key))
  
  (define decryptor
    (λ (any-message origin-public-key)
      ;;; ... decrypt here ...
      any-message))
  
  (define encryptor
    (λ (any-message)
      ;;; ... encrypt here ...
      any-message))
  
  (define mac-validator
    (λ (any-message any-mac)
      ;;; ... generate hmac with key=sk ...
      ;;; (= any-hmac my-hmac)
      #t))
  
  (clan (current-thread) 
        shared-key-calculator
        decryptor
        encryptor
        mac-validator
        my-public-key 
        (base64-url-encode my-public-key)))

;; OPERATIONS

; encrypt: clan bytestring -> bytestring
; encrypt a message with the given clan's credentials
(define (clan-encrypt aclan bstr)
  ((clan-encryptor aclan) bstr (clan-pk aclan)))

; decrypt: clan bytestring bytestring -> bytestring
; decrypt a message from a given public key
; with the given clan's credentials
(define (clan-decrypt aclan bstr any-public-key)
  ((clan-decryptor aclan) bstr any-public-key))

; validate: clan bytestring bytestring -> bytestring
; ensure the data integrity and authenticity of a message
; using the given clan's credentials
(define (clan-validate aclan bstr mac)
  ((clan-mac-valid? aclan) bstr mac))

(provide/contract
 ; construction stuff
 [make-new-clan (-> clan?)]
 [clan? (any/c . -> . boolean?)]
 [clan-pk (clan? . -> . bytes?)]
 [clan-pk-urlencoded (clan? . -> . bytes?)]
 
 ; operations
 [clan-encrypt (clan? bytes? . -> . (or/c #f bytes?))]
 [clan-decrypt (clan? bytes? bytes? . -> . (or/c #f bytes?))]
 [clan-validate (clan? bytes? bytes? . -> . boolean?)])