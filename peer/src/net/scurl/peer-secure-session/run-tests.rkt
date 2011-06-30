#lang racket/base

(require rackunit
         rackunit/text-ui
         
         "crypto-test.rkt"
         "peer-secure-session-msgs-test.rkt"
         "peer-secure-session-test.rkt")

; Test Suite for all session tests.
(define all-tests
  (test-suite
   "secure session tests"
   
   crypto-test
   peer-secure-session-msgs-test
   peer-secure-session-test))

; Run all-tests by default
(run-tests all-tests)