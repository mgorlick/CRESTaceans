#lang racket/base

(require  "../../peer/src/net/tcp-peer.rkt"
          "../../peer/src/api/compilation.rkt"
          "../../peer/src/api/message.rkt"
          racket/function
          (planet "uuid-v4.rkt" ("zitterbewegung" "uuid-v4.plt" 2 0)))
(provide (all-defined-out))


(define (get-public-key sc)
  (string->bytes/utf-8 (path/param-path (list-ref (url-path (string->url (if (scurl? sc) (scurl->string sc) sc))) 1))))

(define-syntax spawn
  (syntax-rules ()
    [(_ body ...)
     (thread (λ () body ...))]))

(define-syntax is?
  (syntax-rules ()
    [(_ v) (curry equal? v)]))

(define (uuid) (make-uuid))

(define (do-spawn rqthread spawn metadata hosturl rpy-to #:compile? [compile? #t])
  (ask/send rqthread "SPAWN" hosturl spawn #:metadata metadata #:reply rpy-to #:compile? compile?))

(define (reply-with-payload rqthread rpy-to payload #:metadata [metadata :no-metadata:])
  (ask/send rqthread "POST" rpy-to payload #:metadata metadata))