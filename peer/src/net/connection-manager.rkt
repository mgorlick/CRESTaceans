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

; manager-has-clan? manager bytestring -> void
; test whether the manager is managing a clan identified by
; the given public key bytestring
(define (manager-has-clan? m pk)
  (hash-has-key? (manager-clans m) pk))

(provide/contract
 [manager? (any/c . -> . boolean?)]
 [make-manager (-> manager?)]
 [manager-register-clan (manager? clan? . -> . void)]
 [manager-unregister-clan (manager? clan? . -> . void)]
 [manager-has-clan? (manager? bytes? . -> . void)])