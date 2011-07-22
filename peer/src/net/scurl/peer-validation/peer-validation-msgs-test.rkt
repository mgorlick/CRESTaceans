#lang racket/base

(require racket/contract racket/list)

(require rackunit
         "depends.rkt"
         "scurl.rkt"
         "scurl-test.rkt"
         "peer-validation-msgs.rkt")

; Define a set of testing values.
(define client-hello-msg-list
  (client-hello-msg 1.0))

(define server-hello-msg-list-timestamp (current-seconds))
(define server-hello-msg-list-nonce #"ZQWERTY")
(define server-hello-msg-list
  (server-hello-msg 1.0 server-hello-msg-list-timestamp server-hello-msg-list-nonce))

(define client-auth-msg-list-timestamp (current-seconds))
(define client-auth-msg-list-nonce #"QEWRQWERNKLWERN")
(define client-auth-msg-list-sig #"l3kqr32krnj;kznjra3;")
(define client-auth-msg-list
  (client-auth-msg scurl3-full
                   client-auth-msg-list-timestamp
                   client-auth-msg-list-nonce
                   client-auth-msg-list-sig))

(define server-auth-msg-list-sig #"LSJKNFKENJKS:ENFS:")
(define server-auth-msg-list
  (server-auth-msg scurl3-full server-auth-msg-list-sig))

(define client-done-msg-list
  (client-done-msg #t))

; Main peer-validation-msgs-test
(define peer-validation-msgs-test
  (test-suite
   "Tests for peer-validation-msgs-test.rkt"
   
   (test-case
    "Test conversion functions for objects to id's"
    
    (check-true (andmap (lambda (key id)
                          (and (= (pkey->integer key) id)
                               (equal? (integer->pkey id) key)))
                        PKEYS
                        PKEY-IDS)
                "pkey->integer or integer->digest failed!")
    (check-true (andmap (lambda (digest id)
                          (and (= (digest->integer digest) id)
                               (equal? (integer->digest id) digest)))
                        DIGESTS
                        DIGEST-IDS)
                "digest->integer or integer->digest failed!")
    (check-true (andmap (lambda (cipher id)
                          (and (= (cipher->integer cipher) id)
                               (equal? (integer->cipher id) cipher)))
                        CIPHERS
                        CIPHER-IDS)
                "cipher->integer or integer->cipher failed!")
    
    (check-equal? (integers->pkeys PKEY-IDS)
                  PKEYS
                  "integers->pkeys does not work.")
    (check-equal? (integers->digests DIGEST-IDS)
                  DIGESTS
                  "integers->digests does not work.")
    (check-equal? (integers->ciphers CIPHER-IDS)
                  CIPHERS
                  "integers->ciphers does not work.")
    )
   
   (test-case
    "Check client-hello-msg"
    
    (check-true (list? client-hello-msg-list)
                "client-hello-msg did not return a list.")
    (check-true (client-hello-msg? client-hello-msg-list)
                "client-hello-msg? did not validate a valid list.")
    (check-true (equal? 1.0 (client-hello-msg-max-version client-hello-msg-list))
                "client-hello-msg-max-version did not return the expected value.")
   )
   
   (test-case
    "Check server-hello-msg"
    
    (check-true (list? server-hello-msg-list)
                "server-hello-msg did not return a list.")
    (check-true (server-hello-msg? server-hello-msg-list)
                "server-hello-msg? did not validate a valid list.")
    (check-true (equal? 1.0 (server-hello-msg-version server-hello-msg-list))
                "server-hello-msg-version did not return the expected value.")
    (check-true (equal? server-hello-msg-list-nonce (server-hello-msg-nonce server-hello-msg-list))
                "server-hello-msg-nonce did not return the expected value.")
   )
   
   (test-case
    "Check client-auth-msg"
    
    (check-true (list? client-auth-msg-list)
                "client-auth-msg did not return a list.")
    (check-true (client-auth-msg? client-auth-msg-list)
                "client-auth-msg? did not validate a valid list.")
    (check-true (scurl=? (client-auth-msg-scurl client-auth-msg-list)
                         scurl3-full)
                "client-auth-msg-scurl did not return the correct scurl.")
    (check-true (equal? client-auth-msg-list-timestamp (client-auth-msg-timestamp client-auth-msg-list))
                "client-auth-msg-timestamp did not return the expected value.")
    (check-true (equal? client-auth-msg-list-nonce (client-auth-msg-nonce client-auth-msg-list))
                "client-auth-msg-nonce did not return the expected value.")
    (check-true (equal? client-auth-msg-list-sig (client-auth-msg-signature client-auth-msg-list))
                "client-auth-msg-signature did not return the expected value.")
   )
   
   (test-case
    "Check server-auth-msg"
    
    (check-true (list? server-auth-msg-list)
                "server-auth-msg did not return a list.")
    (check-true (server-auth-msg? server-auth-msg-list)
                "server-auth-msg? did not validate a valid list.")
    (check-true (scurl=? (server-auth-msg-scurl server-auth-msg-list)
                         scurl3-full)
                "server-auth-msg-scurl did not return the correct scurl.")
    (check-true (equal? server-auth-msg-list-sig (server-auth-msg-signature server-auth-msg-list))
                "server-auth-msg-signature did not return the expected value.")
    )
   
   (test-case
    "Check client-done-msg"
    
    (check-true (list? client-done-msg-list)
                "client-done-msg did not return a list.")
    (check-true (client-done-msg? client-done-msg-list)
                "client-done-msg? did not validate a valid list.")
    (check-true (equal? #t (client-done-msg-is-authenticated client-done-msg-list))
                "client-done-msg-is-authenticated did not return the expected value.")
    )
   
   ))

; Provide everything
(provide (all-defined-out))

; Uncomment to run this test from this file.
;(require rackunit/text-ui)
;(run-tests peer-validation-msgs-test)