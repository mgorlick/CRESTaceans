#lang racket/base

(require racket/contract
         "scurl/peer-validation/depends.rkt"
         "scurl/peer-validation/scurl.rkt"
         "scurl/peer-validation/peer-validation.rkt")
(provide (all-defined-out)
         (all-from-out "scurl/peer-validation/depends.rkt"
                       "scurl/peer-validation/scurl.rkt"
                       "scurl/peer-validation/peer-validation.rkt"))

;; SCURL generation and recreation as used by tcp-peer.
;; this module re-exports its imports at the moment; this may not be strictly necessary in the future.

(define/contract (do-server-auth this-scurl revoke? i o)
  (scurl? (scurl? . -> . boolean?) input-port? output-port? . -> . (values (or/c #f string?) (or/c #f exact-nonnegative-integer?)))
  (define the-remote-scurl (handle-server-authentication this-scurl revoke? i o))
  (flush-output o)
  (cond [(scurl? the-remote-scurl)
         (define the-remote-url (scurl-url the-remote-scurl))
         (define ra (url-host the-remote-url))
         (define rp (url-port the-remote-url))
         (values ra rp)]
        [else (printf "not accepted: the returned scurl auth is ~a~n" the-remote-scurl)
              (values #f #f)]))

(define/contract (do-client-auth host port key this-scurl i o)
  (string? exact-nonnegative-integer? bytes? scurl? input-port? output-port? . -> . (or/c #f scurl?))
  ;; construct a SCURL expressing the intent of our connection request.
  ;; if the SCURL library returns a SCURL then we know the validation protocol succeeded.
  (define should-be-scurl (triple->scurl/defaults host port key))
  (define the-remote-scurl (handle-client-authentication this-scurl should-be-scurl i o))
  (flush-output o)
  (and (scurl? the-remote-scurl) the-remote-scurl))

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