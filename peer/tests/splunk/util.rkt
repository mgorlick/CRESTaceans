#lang racket

(require "../../src/splunk/util.rkt"
         rackunit
         rackunit/text-ui)

(define s
  (test-suite
   "Tests for splunk utility functions"
   (test-case
    "Ensure data is correctly urlencoded"
    (let ([data '((key1 . value1) (key2 . value2))])
      (check-equal? "key1=value1&key2=value2" (make-wwwform-post-data data))))
   (test-case
    "CRLF etc are stripped"
    (check-equal? #"HelloWorld" (strip-crlf #"Hello\rWor\r\nld\t   ")))
   )
  )

(run-tests s)