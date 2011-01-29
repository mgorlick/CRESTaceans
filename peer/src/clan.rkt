#lang racket

(require "net/base64-url-typed.rkt"
         (planet vyzo/crypto))

;; CONSTRUCTION
(define-struct/contract 
  clan 
  ([thd thread?] ; reference to thread the clan is running in
   [encrypter (bytes? bytes? . -> . (values bytes? bytes?))]
   [decrypter (bytes? bytes? bytes? . -> . bytes?)]
   [mac-calculator (bytes? bytes? . -> . bytes?)]
   [mac-validator (bytes? bytes? bytes? . -> . boolean?)]
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
  (define *digest-used* digest:sha512)
  
  ; my-public-key is the key that will appear in the URLs that name members of this clan
  ; my-private-key should never leave the lexical scope of `make-new-clan'!
  (define-values (my-private-key my-public-key) (generate-key dh:1024))
  
  (define (make-memoized-key-calculator)
    (let ([memoized-shared-keys (make-hash)])
      (λ (any-public-key)
        (hash-ref memoized-shared-keys any-public-key
                  (λ ()
                    (let ([shared-key (compute-key my-private-key any-public-key)])
                      (hash-set! memoized-shared-keys any-public-key shared-key)
                      shared-key))))))
  
  ; shared-key-calculator: bytestring -> bytestring
  ; a procedure that calculates a shared key with a
  ; given remote public key using the private key
  (define shared-key-calculator (make-memoized-key-calculator))
  
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
  
  (define (make-memoized-mac-calculator)
    (let ([memoized-macs (make-hash)])
      (λ (any-message remote-public-key)
        (hash-ref memoized-macs (cons any-message remote-public-key)
                  (λ ()
                    (let* ([shared-key (shared-key-calculator remote-public-key)]
                           [mac (hmac *digest-used* shared-key any-message)])
                      (hash-set! memoized-macs (cons any-message remote-public-key) mac)
                      mac))))))
  
  ; mac-calculator: bytes bytes -> bytes
  ; calculate the mac of a message using the recipient's public key
  (define mac-calculator (make-memoized-mac-calculator))
  
  ; mac-validator: bytes bytes bytes -> boolean
  ; verify that the mac sent is valid with respect to the message sent
  (define (mac-validator any-message origin-public-key any-mac)
    (bytes=? any-mac (mac-calculator any-message origin-public-key)))
  
  (clan (current-thread) 
        encrypter
        decrypter
        mac-calculator
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

; clan-mac-valid?: clan bytestring bytestring -> bytestring
; ensure the data integrity and authenticity of a message
; using the given clan's credentials
(define (clan-mac-valid? aclan bstr origin-public-key mac)
  ((clan-mac-validator aclan) bstr origin-public-key mac))

; clan-mac-calc: clan bytestring bytestring -> bytestring
; calculate the MAC for a given message using the public key
; of the recipient
(define (clan-mac-calc aclan bstr recipient-public-key)
  ((clan-mac-calculator aclan) bstr recipient-public-key))

(provide/contract
 ; construction stuff
 [make-new-clan (-> clan?)]
 [clan? (any/c . -> . boolean?)]
 [clan-pk (clan? . -> . bytes?)]
 [clan-pk-urlencoded (clan? . -> . bytes?)]
 
 ; operations
 [clan-encrypt (clan? bytes? bytes? . -> . (values bytes? bytes?))]
 [clan-decrypt (clan? bytes? bytes? bytes? . -> . (or/c #f bytes?))]
 [clan-mac-valid? (clan? bytes? bytes? bytes? . -> . boolean?)]
 [clan-mac-calc (clan? bytes? bytes? . -> . bytes?)])