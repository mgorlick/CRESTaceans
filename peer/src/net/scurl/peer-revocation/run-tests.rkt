#lang racket/base

(require rackunit
         rackunit/text-ui
         "depends.rkt"
         "revo-cert-test.rkt"
         "revocation-msgs-test.rkt"
         "peer-revocation-test.rkt")

; Test Suite for all revocation tests.
(define all-tests
  (test-suite
   "revocation tests"
   
   revo-cert-test
   revocation-msgs-test
   peer-revocation-test))

; Run all-tests by default
(run-tests all-tests)
