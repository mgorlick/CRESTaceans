#lang racket

(require "../src/clan.rkt"
         rackunit
         rackunit/text-ui)

(define key-tests
  (test-suite
   "Tests for Diffie-Hellman key validation"
   (test-case
    "The two computed keys should be equal"
    (define bob (make-new-clan))
    (define alice (make-new-clan))
    (check-equal? (compute-shared-key bob (clan-pk alice))
                  (compute-shared-key alice (clan-pk bob))))
   ))

(run-tests key-tests)