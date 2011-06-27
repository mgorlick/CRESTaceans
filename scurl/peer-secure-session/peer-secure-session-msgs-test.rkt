#lang racket/base

(require rackunit
         "depends.rkt"
         "crypto.rkt"
         "peer-secure-session-msgs.rkt"
         
         "../peer-validation/scurl-test.rkt")

; Define a set of testing values.
(define secure-connection-options-request-msg-list
  (secure-connection-options-request-msg))

(define secure-connection-options-response-msg-list
  (secure-connection-options-response-msg))

(define secure-connection-request-msg-list
  (secure-connection-request-msg cipher:aes-256))

(define secure-connection-response-msg-list
  (secure-connection-response-msg cipher:aes-256))

; Main peer-secure-session-msgs-test
(define peer-secure-session-msgs-test
  (test-suite
   "Tests for peer-secure-session-msgs-test.rkt"
   
   (test-case
    "Check secure-connection-options-request-msg"
    
    (check-true (list? secure-connection-options-request-msg-list)
                "secure-connection-options-request-msg did not return a list.")
    (check-true (secure-connection-options-request-msg? secure-connection-options-request-msg-list)
                "secure-connection-options-request-msg? did not validate a valid list.")
   )
   
   (test-case
    "Check secure-connection-options-response-msg"
    
    (check-true (list? secure-connection-options-response-msg-list)
                "secure-connection-options-response-msg did not return a list.")
    (check-true (secure-connection-options-response-msg? secure-connection-options-response-msg-list)
                "secure-connection-options-response-msg? did not validate a valid list.")
    (check-true (and
                 (list? (secure-connection-options-response-msg-ciphers secure-connection-options-response-msg-list))
                 (andmap !cipher? (secure-connection-options-response-msg-ciphers secure-connection-options-response-msg-list)))
                "secure-connection-options-response-msg-ciphers did not return the correct value.")
    )
   
   (test-case
    "Check secure-connection-request-msg"
    
    (check-true (list? secure-connection-request-msg-list)
                "secure-connection-request-msg did not return a list.")
    (check-true (secure-connection-request-msg? secure-connection-request-msg-list)
                "secure-connection-request-msg? did not validate a valid list.")
    (check-true (!cipher? (secure-connection-request-msg-cipher secure-connection-request-msg-list))
                "secure-connection-request-msg-cipher did not return a valid cipher type.")
    (check-true (shared-keys? (secure-connection-request-msg-shared-keys secure-connection-request-msg-list))
                "secure-connection-request-msg-shared-keys did not return a valid shared-keys type.")
    )
   
   (test-case
    "Check secure-connection-response-msg"
    
    (check-true (list? secure-connection-response-msg-list)
                "secure-connection-response-msg did not return a list.")
    (check-true (secure-connection-response-msg? secure-connection-response-msg-list)
                "secure-connection-response-msg? did not validate a valid list.")
    (check-true (shared-keys? (secure-connection-response-msg-shared-keys secure-connection-response-msg-list))
                "secure-connection-response-msg-shared-keys did not return a shared-keys struct.")
    )
   
   ))

; Provide everything
(provide (all-defined-out))

; Uncomment to run this test from this file.
;(require rackunit/text-ui)
;(run-tests peer-secure-session-msgs-test)
