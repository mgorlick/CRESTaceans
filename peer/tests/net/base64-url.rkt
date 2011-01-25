#lang racket

(require "../../src/net/base64-url.rkt"
         rackunit
         rackunit/text-ui
         (planet vyzo/crypto))

(define dencode-tests
  (test-suite
   "Tests of base64 encoding and decoding"
   (test-case
    "encode and decode are inverse functions"
    (let-values ([(priv pub) (generate-key dh:1024)])
      (let ([key (compute-key priv pub)])
        (check-equal? key (base64-url-decode (base64-url-encode key))))))
   (test-case
    "encode produces strings that are base64-url-encoded?"
    (let-values ([(priv pub) (generate-key dh:1024)])
      (let ([key (compute-key priv pub)])
        (check-pred base64-url-encoded? (bytes->string/utf-8 (base64-url-encode key))))))
   ))

(run-tests dencode-tests)