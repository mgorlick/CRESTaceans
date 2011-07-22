#lang racket/base

(require racket/contract racket/list)

(require rackunit
         rackunit/text-ui
         "depends.rkt"
         "host-id-test.rkt"
         "scurl-test.rkt"
         "peer-validation-msgs-test.rkt"
         "scurl-utils-test.rkt"
         "peer-validation-test.rkt")

; Test Suite for all session tests.
(define all-tests
  (test-suite
   "peer validation tests"
   
   host-id-test
   scurl-test
   scurl-utils-test
   peer-validation-msgs-test
   peer-validation-test))

; Run all-tests by default
(run-tests all-tests)
