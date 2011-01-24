#lang racket

(require "../../src/net/url.rkt"
         rackunit
         rackunit/text-ui)

(define url-tests
  (test-suite 
   "Check CREST URLs encoding and decoding" 
   (test-case
    "Verify base64-URL-encoding recognizer"
    (check-pred base64-url-encoded? "Abcd1234")
    (check-pred base64-url-encoded? "Abcd-")
    (check-pred base64-url-encoded? "Abcd_")
    (check-pred false? (base64-url-encoded? "++333"))
   )
  (test-case
   "Verify number recognizer"
   (check-pred isnum? "1234")
   (check-pred false? (isnum? "ab525")))
  (test-case
    "URL construction rejects non-base64-encoded public key"
    (let ([u (string->crest-url "crest://bob.org:8000/++333/226155721/path;add2num?x=5&y=4#cont")])
      (check-pred false? u)))
   (test-case
    "URL construction rejects non-numeric swiss number"
    (let ([u (string->crest-url "crest://bob.org:8000/9998ec7e/ab525/path;add2num?x=5&y=4#cont")])
      (check-pred false? u)))
   (test-case
    "string->crest-url and crest-url->string are inverse functions"
    (let* ([u "crest://bob.org:8000/9998ec7e/225555721/path;add2num?x=5&y=4#cont"]
           [u2 (string->crest-url u)]
           [u3 (crest-url->string u2)])
      (check-equal? u u3)))
  ))

(run-tests url-tests)