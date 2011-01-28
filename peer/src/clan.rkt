#lang racket

(require "net/base64-url.rkt"
         (planet vyzo/crypto))

;; CONSTRUCTION
(define-struct/contract 
  clan 
  ([thd thread?] ; reference to thread the clan is running in
   [encrypter (bytes? bytes? . -> . (values bytes? bytes?))]
   [decrypter (bytes? bytes? bytes? . -> . bytes?)]
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
  
  (define *cipher-used* cipher:aes-256)
  
  ; my-public-key is the key that will appear in the URLs that name members of this clan
  ; my-private-key should never leave the lexical scope of `make-new-clan'!
  (define-values (my-private-key my-public-key) (generate-key dh:1024))
  
  ; shared-key-calculator: bytestring -> bytestring
  ; a procedure that calculates a shared key with a
  ; given remote public key using the private key
  (define shared-key-calculator (curry compute-key my-private-key))
  
  (define (docipher cipher any-message)
    (bytes-append (cipher-update! cipher any-message)
                  (cipher-final! cipher)))
  
  ; encrypter: bytestring bytestring -> bytestring
  ; encrypt the message using the public key of the intended recipient
  (define (encrypter any-message recipient-public-key)
    (let-values ([(_ iv) (generate-key *cipher-used*)])
      (let* ([shared-key (shared-key-calculator recipient-public-key)]
             [cipher (cipher-encrypt *cipher-used* shared-key iv)]
             [encrypted-message (docipher cipher any-message)])
        (values encrypted-message iv))))
  
  ; decrypter: bytestring bytestring bytestring -> bytestring
  ; decrypt the message using the origin public key and the iv originally
  ; used to encrypt it. only do this after verifying the MAC
  (define (decrypter encrypted-message origin-public-key iv-used)
    (let* ([shared-key (shared-key-calculator origin-public-key)]
           [cipher (cipher-decrypt *cipher-used* shared-key iv-used)])
      (docipher cipher encrypted-message)))
  
  (define (mac-calculator any-message recipient-public-key)
    ; ...
    #t)
  
  ; mac-validator:
  (define (mac-validator any-message origin-public-key)
    ;;; ... generate hmac with key=sk ...
    ;;; (= any-hmac my-hmac)
    #t)
  
  (clan (current-thread) 
        encrypter
        decrypter
        mac-validator
        my-public-key 
        (base64-url-encode my-public-key)))

;; OPERATIONS

; encrypt: clan bytestring -> (values bytestring bytestring)
; encrypt a message with the given clan's credentials
; returns the encrypted message and the initialization vector
; used to encrypt it
(define (clan-encrypt aclan msg recipient-public-key)
  ((clan-encrypter aclan) msg recipient-public-key))

; decrypt: clan bytestring bytestring bytestring -> bytestring
; decrypt a message from a given public key
; and the initialization vector used to encrypt it
; with the given clan's credentials
(define (clan-decrypt aclan msg origin-public-key iv-used)
  ((clan-decrypter aclan) msg origin-public-key iv-used))

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
 [clan-encrypt (clan? bytes? bytes? . -> . (values bytes? bytes?))]
 [clan-decrypt (clan? bytes? bytes? bytes? . -> . (or/c #f bytes?))]
 [clan-validate (clan? bytes? bytes? . -> . boolean?)])