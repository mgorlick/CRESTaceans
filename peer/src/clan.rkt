#lang racket

(require "net/base64-url.rkt"
         (planet vyzo/crypto))

; a clan is a struct
; with a thread identifier (clans only run in one thread),
; a procedure to calculate the key the clan shares with
; another clan (Diffie-Hellman), and a public key bytestring
(struct clan (thd skcalc pk pk-urlencoded))

; make-new-clan: -> clan
; create a clan with a randomly generated public and private key
; that is ready to calculate shared keys given another clan's key
; and validate a presented shared key
; precompute the encoded public shared key to avoid encoding it over and over again later
(define (make-new-clan)
  (let-values ([(my-private-key my-public-key) (generate-key dh:1024)])
    (clan (current-thread) 
          (make-shared-key-calculator my-private-key)
          my-public-key 
          (base64-url-encode my-public-key))))

; make-shared-key-calculator: a:bytestring b:bytestring -> bytestring
; return a procedure that, given the local public key a and the
; local private key b, calculates a shared key with a
; given remote public key using the private key b
(define (make-shared-key-calculator my-private-key)
  (λ (any-public-key) ; any-shared-key will come from the URL of the other peer
    (let ([our-shared-key (compute-key my-private-key any-public-key)])
      our-shared-key)))

; compute-shared-key: clan bytestring -> bytestring
; produce the shared key resulting from the Diffie-Hellman key exchange
; between the clan and another clan supplying a public key
(define (compute-shared-key aclan any-public-key)
  ((clan-skcalc aclan) any-public-key))

; validate-shared-key: clan bytestring bytestring -> boolean
; test whether the presented key is correct according to Diffie-Hellman
(define (validate-shared-key aclan any-public-key any-shared-key)
  (bytes=? (compute-shared-key aclan any-public-key) any-shared-key))

(provide/contract
 [clan? (any/c . -> . boolean?)]
 [clan-pk (clan? . -> . bytes?)]
 [clan-pk-urlencoded (clan? . -> . bytes?)]
 [make-new-clan (-> clan?)]
 [compute-shared-key (clan? bytes? . -> . bytes?)]
 [validate-shared-key (clan? bytes? bytes? . -> . boolean?)])