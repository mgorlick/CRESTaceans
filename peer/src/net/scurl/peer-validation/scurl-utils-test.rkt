#lang racket/base

(require racket/contract racket/list)

(require rackunit
         "depends.rkt"
         "scurl.rkt"
         "scurl-test.rkt"
         "scurl-utils.rkt")

; Main scurl test.
(define scurl-utils-test
  (test-suite
   "Tests for scurl-utils.rkt"
   
   ; Test scurl creation.
   (test-case 
    "Test conversion utilities."
    
    (check-equal? (scurl->strings scurl3-full) (scurl->strings scurl3-full)
                  "scurl->strings does not act as expected.")
    (check-equal? (scurl->strings scurl3-full-public) (scurl->strings scurl3-full-public)
                  "scurl->strings does not act as expected.")
    (check-not-equal? (scurl->strings scurl3-full) (scurl->strings scurl4-full)
                      "scurl->strings does not act as expected.")
    
    (check-true (scurl=? (strings->scurl (scurl->strings scurl3-full)) scurl3-full)
                "strings->scurl does not act as expected.")
    (check-true (scurl=? (strings->scurl (scurl->strings scurl3-full-public)) scurl3-full-public)
                "strings->scurl does not act as expected.")
    (check-false (scurl=? (strings->scurl (scurl->strings scurl3-full)) scurl4-full)
                 "strings->scurl does not act as expected.")
    
    (letrec ((s1 (generate-scurl "http://www.amazon.com:3456/index.html"
                                 digest:sha256
                                 pkey:rsa
                                 (generate-key pkey:rsa 1024)))
             (s2 (generate-scurl "http://www.ebay.com:3456/index.html"
                                 digest:sha256
                                 pkey:rsa
                                 (generate-key pkey:rsa 2056)))
             (s3 (generate-scurl "http://www.google.com"
                                 digest:sha256
                                 pkey:rsa
                                 (generate-key pkey:rsa 512)))
             (s-list (list s1 s2 s3)))
      
      (check-true (let ((string-list (scurl->strings s1)))
                    (and (list? string-list)
                         (andmap string? string-list)))
                  "scurl->strings did not produce a list of strings.")
      (check-true (let ((string-list (scurl->strings s2)))
                    (and (list? string-list)
                         (andmap string? string-list)))
                  "scurl->strings did not produce a list of strings.")
      (check-true (let ((string-list (scurl->strings s3)))
                    (and (list? string-list)
                         (andmap string? string-list)))
                  "scurl->strings did not produce a list of strings.")
      
      (check-true (scurl? (strings->scurl (scurl->strings s1)))
                  "strings->scurl does not produce a valid scurl.")
      (check-true (scurl? (strings->scurl (scurl->strings s2)))
                  "strings->scurl does not produce a valid scurl.")
      (check-true (scurl? (strings->scurl (scurl->strings s3)))
                  "strings->scurl does not produce a valid scurl.")
    ))))

; Provide everything.
(provide (all-defined-out))

; Uncomment to run this test from this file.
;(require rackunit/text-ui)
;(run-tests scurl-utils-test)