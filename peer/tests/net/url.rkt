#lang racket

(require "../../src/net/url.rkt")
;(test-suite 
; "Check CREST URLs encoding and decoding"
; 
; (test-case
 
(define u (string->crest-url "crest://alice@bob.org:8000/998ecf8427e/226155721/application/path;add2num;doitfast=please;hurryup?x=5;y=4#cont"))

u
(crest-url->string u)
(crest-url-host u)
(crest-url-port u)
(crest-url-path u)
(crest-url-pathstring u)