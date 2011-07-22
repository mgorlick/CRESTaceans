#lang racket/base

(require racket/contract racket/list)

(require rackunit
         rackunit/text-ui
         "depends.rkt"
         
         "program-test.rkt"
         "certification-test.rkt"
         "revocation-test.rkt"
         "forward-proxy-test.rkt"
         "reverse-proxy-test.rkt")

; Test Suite for all services tests.
(define all-tests
  (test-suite
   "services tests"
   
   program-test
   certification-test
   revocation-test
   forward-proxy-test
   reverse-proxy-test))

; Run all-tests by default
(run-tests all-tests)
