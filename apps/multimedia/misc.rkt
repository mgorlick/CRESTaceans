#lang racket/base

(require  "../../peer/src/net/tcp-peer.rkt"
          "../../peer/src/net/structs.rkt"
          "../../peer/src/api/compilation.rkt"
          "../../peer/src/api/message.rkt"
          racket/function)
(provide (all-defined-out))


(define (get-public-key sc)
  (path/param-path (list-ref (url-path (string->url (if (scurl? sc) (scurl->string sc) sc))) 1)))

(define-syntax spawn
  (syntax-rules ()
    [(_ body ...)
     (thread (λ () body ...))]))

(define-syntax is?
  (syntax-rules ()
    [(_ v) (curry equal? v)]))