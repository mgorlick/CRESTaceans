#lang racket/base

(require rackunit
         "host-id.rkt")

; Define a set of testing values.
(define
  compressed-host-id
  (make-bytes 20 255))

(define
  expanded-host-id
  (make-bytes 32 31))

(define
  test-host-id-z
  "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")

(define
  test-host-id-full
  "23456789abcdefghijkmnpqrstuvwxyz")

; Main host-id test.
(define host-id-test
  (test-suite
   "Tests for host-id.rkt"
   
   ; Simple test cases
   (test-case
    "Simple type checks"
    
    (check-true (host-id-string? (make-string (host-id-string-length) #\z))
                "Valid host-id in string form failed to pass host-id-string?")
    (check-false (host-id-string? (make-string (- (host-id-string-length) 1) #\a))
                 "Invalid host-id in string form passed host-id-string?")
    (check-true (host-id-bytes? (make-bytes (host-id-bytes-length) 15))
                "Valid host-id in bytes form failed to pass host-id-bytes?")
    (check-false (host-id-bytes? (make-bytes (- (host-id-bytes-length) 1) 15))
                 "Invalid host-id in bytes form passed host-id-bytes?"))
   
   ; Test byte expansion/compression.
   (test-case 
    "Test byte expansion and compression"
    
    (check-equal? (compress-host-id expanded-host-id) compressed-host-id "compress-host-id produces correct output.")
    (check-equal? (expand-host-id compressed-host-id) expanded-host-id "expand-host-id produces correct output.")
    (check-equal? (expand-host-id (compress-host-id expanded-host-id)) expanded-host-id "Round trip compress->expand.")
    (check-equal? (compress-host-id (expand-host-id compressed-host-id)) compressed-host-id "Round trip expand->compress."))
   
   ; Test host-id-string->host-id-bytes
   (test-case 
    "Test string to byte conversion."
    
    (check-equal? (host-id-string->host-id-bytes test-host-id-z) compressed-host-id
                  "string->bytes of z's is all value 31.")
    (check-equal? (host-id-bytes->host-id-string compressed-host-id) test-host-id-z
                  "bytes->string of value 31 is all z's.")
    (check-equal? (host-id-bytes->host-id-string (host-id-string->host-id-bytes test-host-id-full)) test-host-id-full
                  "Round trip bytes->string.")
    (check-equal? (host-id-string->host-id-bytes (host-id-bytes->host-id-string compressed-host-id)) compressed-host-id
                  "Round trip string->bytes."))))

; Provide everything
(provide (all-defined-out))

; Uncomment to run this test from this file.
;(require rackunit/text-ui)
;(run-tests host-id-test)