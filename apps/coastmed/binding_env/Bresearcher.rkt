#lang racket/base

(require COAST
         "../utils.rkt"
         "bindings-extensions.rkt")

(provide (all-defined-out))

;(define (request-records)
  ;'hospitalEHR

; defines the binding environment βrequestEHR for actor Arequest
(define βrequestEHR (pairs/environ (environs/merge BASELINE MESSAGES/RECEIVE DISPLAYING MESSAGES/SEND) 
                                   (global-defines sleep curl/get-meta delivery/curl-used expiration/date->milliseconds curl/new/any locative/cons/any this/locative)))


(define keychain (make-hash))

(define (store-curl key value)
  (hash-set! keychain key value))

(define (retrieve-curl key)
  (hash-ref keychain key #f))

; defines the binding environment βkeychain for actor Akeychain
(define βkeychain (pairs/environ (environs/merge BASELINE MESSAGES/SEND MESSAGES/RECEIVE DISPLAYING) 
                                 (global-defines curl? curl/get-meta keychain store-curl retrieve-curl)))

