#lang racket/base

(require rackunit

         "revo-cert.rkt"
         "revo-cert-test.rkt"
         "revocation-msgs.rkt")

; Define a set of testing values.
(define key-revocation-response-msg-list
  (key-revocation-response-msg revo-cert3))

; Main revocation-msgs-test
(define revocation-msgs-test
  (test-suite
   "Tests for revocation-msgs-test.rkt"
   
   (test-case
    "Check key-revocation-response-msg"
    
    (check-true (list? key-revocation-response-msg-list)
                "key-revocation-response-msg did not return a list.")
    (check-true (key-revocation-response-msg? key-revocation-response-msg-list)
                "key-revocation-response-msg? did not validate a valid list.")
    (check-true (revo-cert? (key-revocation-response-msg-revo-cert key-revocation-response-msg-list))
                "key-revocation-response-msg-revo-cert did not return a revo-cert.")
    )
   )
  )

; Provide everything
(provide (all-defined-out))

; Uncomment to run this test from this file.
;(require rackunit/text-ui)
;(run-tests revocation-msgs-test)