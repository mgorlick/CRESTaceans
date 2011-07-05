#lang racket/base

(require "scurl/peer-validation/depends.rkt"
         "scurl/peer-validation/scurl.rkt"
         "scurl/peer-validation/peer-validation.rkt")
(provide (all-defined-out)
         (all-from-out "scurl/peer-validation/depends.rkt"
                       "scurl/peer-validation/scurl.rkt"
                       "scurl/peer-validation/peer-validation.rkt"))

;; SCURL generation and recreation as used by tcp-peer.
;; this module re-exports its imports at the moment; this may not be strictly necessary in the future.

;; right now, just use defaults and don't negotiate.
(define *DIGEST-TYPE* digest:sha512)
(define *KEY-TYPE* pkey:rsa)
(define *KEY-LEN* 2048)

(define (generate-key/defaults)
  (generate-key *KEY-TYPE* *KEY-LEN*))

;; use generate-scurl/defaults when constructing URLs referring to self
(define (generate-scurl/defaults hostname port #:path [path ""] #:key [key (generate-key/defaults)])
  (generate-scurl (format "imp://~a:~a/~a" hostname port path)
                  *DIGEST-TYPE* *KEY-TYPE* key))

;; use triple->scurl/defaults when connecting to a known host:port:PK triple
;; (not useful for generating, as opposed to verifying, since it's never backed by a private key)
(define (triple->scurl/defaults hostname port key)
  (string->scurl (format "imp://~a:~a/scurl/~a" hostname port key) *DIGEST-TYPE* *KEY-TYPE*))