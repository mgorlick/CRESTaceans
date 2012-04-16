#! /usr/bin/env racket
#lang racket/base

(require "config.rkt"
         "../../peer/src/net/tcp-peer.rkt"         
         "../../Motile/persistent/hash.rkt"
         "../../Motile/compile/serialize.rkt"
         "../../Motile/generate/baseline.rkt"
         "../../Motile/actor/actor.rkt"
         "../../Motile/actor/curl.rkt"
         "../../Motile/actor/send.rkt"
         "../../Motile/actor/delivery.rkt"
         "../../Motile/actor/promise.rkt"
         "../../Motile/actor/jumpstart.rkt"
         "../../Motile/actor/island.rkt"
         "../../Motile/actor/locative.rkt"
         "../../Motile/actor/logger.rkt"
         racket/function
         racket/list
         racket/match
         racket/contract
         racket/dict)

(provide (all-defined-out))

(define-syntax-rule (->boolean expr)
  (if expr
      #t
      #f))

(define/contract (curl/known-public? host port)
  ((or/c string? bytes?) exact-nonnegative-integer? . -> . boolean?)
  (->boolean (dict-ref PUBLICS (cons (if (string? host) (string->bytes/utf-8 host) host) port) #f)))

(define/contract (curl/get-public host port)
  ((or/c string? bytes?) exact-nonnegative-integer? . -> . curl?)
  (motile/deserialize (dict-ref PUBLICS (cons (if (string? host) (string->bytes/utf-8 host) host)
                                              port)
                                #f) #f))

(define (make-root/get-public/register-public)
  (define-values (root root-locative) (actor/root/new))
  (define public-locative (locative/cons/any root-locative 2.76e110 2.76e110 #t #t))
  (locative/id! public-locative '(public))
  (define public-curl (curl/new/any public-locative null #f))
  (motile/serialize public-curl) ; put in exports table.
  (values root root-locative 
          public-locative 
          (if (curl/known-public? (island/address/dns (this/island)) (island/address/port (this/island)))
              (curl/get-public (island/address/dns (this/island)) (island/address/port (this/island)))
              public-curl)
          ))

;; put these serialized curls into config.rkt

(for/list ([i (in-range 5000 5020)])
  (this/island (island/address/new #"abcdefghijklmnopqrstuvwxyz" #"128.195.59.199" i))
  (define-values (root rootl publicl publicc) (make-root/get-public/register-public))
 `((,(island/address/dns (this/island)) . ,i) . (,(motile/serialize (curl/new/any publicl null #f)))))