#lang racket/base

(require racket/contract racket/list)

(require rackunit

         "revo-cert.rkt"
         "../peer-validation/scurl-test.rkt")

; Define a set of testing values.
(define revo-cert3 (scurl->revo-cert scurl3-full))
(define revo-cert3-bad (revo-cert scurl3-full #"IAMABADSIGNATURE"))
(define revo-cert4 (scurl->revo-cert scurl4-full))

; Main revo-cert-test
(define revo-cert-test
  (test-suite
   "Tests for revocation-test.rkt"
   
   (test-case
    "Check the revocation-certificate structure and functions."
    
    (check-true (revo-cert? revo-cert3)
                "scurl->revo-cert did not return a revocation certificate for scurl3-full.")
    (check-true (revo-cert? revo-cert4)
                "scurl->revo-cert did not return a revocation certificate for scurl4-full.")
    (check-true (revo-cert=? revo-cert3 revo-cert3)
                "revo-cert=? did not validate the same two revocation certificates.")
    (check-false (revo-cert=? revo-cert3 revo-cert4)
                 "revo-cert=? validated two different revocation certificates.")
    (check-true (valid-revo-cert? revo-cert3)
                "Failed to validate a valid revocation certificate through the valid-revo-cert? function.")
    (check-false (valid-revo-cert? revo-cert3-bad)
                 "Validated a bogus revocation certificate with the valid-revo-cert? function.")
    
    )
   )
  )

; Provide everything
(provide (all-defined-out))

; Uncomment to run this test from this file.
;(require rackunit/text-ui)
;(run-tests revo-cert-test)
