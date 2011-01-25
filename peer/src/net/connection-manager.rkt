#lang racket

(require "../clan.rkt")

(struct manager (clans))

; make-manager: -> manager
; produce a new manager with no clans registered
(define (make-manager) 
  (manager (make-hash)))

; manager-register-clan: manager clan -> void
; use the clan's public key to register it with the manager.
; the clan will subsequently receive valid (authenticated)
; messages directed toward peers identified with the clan's
; public key
(define (manager-register-clan m aclan)
  (hash-set! (manager-clans m) (clan-pk aclan) aclan))

; manager-unregister-clan: manager clan -> void
; remove the clan from the manager's registry, ensuring
; that it will no longer receive messages toward its peers
(define (manager-unregister-clan m aclan)
  (hash-remove! (manager-clans m) (clan-pk aclan)))

; manager-calc-shared-key: manager bytestring bytestring -> bytestring or #f
; given the public key of a local clan and the public key of a remote clan,
; validate generate the shared key that should be used to authenticate
; one to the other
(define (manager-calc-shared-key m clan-public-key request-public-key)
  (let ([clans (manager-clans m)])
    (cond
      [(hash-has-key? clans clan-public-key)
       (compute-shared-key (hash-ref clans clan-public-key) request-public-key)]
      [else #f])))

; manager-key-valid?: manager bytestring bytestring bytestring -> boolean
; given the local public key, remote public key and remotely-calculated shared key,
; test the remotely-calculated shared key for equality with the local clan's
; expectation of what the shared key should be
(define (manager-key-valid? m clan-public-key request-public-key request-shared-key)
  (let ([clans (manager-clans m)])
    (cond
      [(hash-has-key? clans clan-public-key)
       (validate-shared-key (hash-ref clans clan-public-key) request-public-key request-shared-key)]
      [else #f])))

(provide/contract
 [manager? (any/c . -> . boolean?)]
 [make-manager (-> manager?)]
 [manager-register-clan (manager? clan? . -> . void)]
 [manager-unregister-clan (manager? clan? . -> . void)]
 [manager-calc-shared-key (manager? bytes? bytes? . -> . (or/c #f bytes?))]
 [manager-key-valid? (manager? bytes? bytes? bytes? . -> . boolean?)])