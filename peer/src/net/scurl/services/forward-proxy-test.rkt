#lang racket/base

(require rackunit

         "depends.rkt"
         "forward-proxy.rkt"
         "program.rkt"
         
         net/url
         "../peer-validation/scurl.rkt")

; Main forward-proxy-test
(define forward-proxy-test
  (test-suite
   "Tests for forward-proxy-test.rkt"
   
   (test-case
    "Check the run-fproxy-programs function."
    
    ; Test that string, url, scurl and others are handled correctly when adding a program.
    (let* ((prog-list (add-fproxy-program (new-program-list) "http://www.amazon.com/1" #:filter ".*first.*"))
           (prog-list (add-fproxy-program prog-list (string->url "http://www.amazon.com/2") #:filter ".*second.*"))
           (3s (generate-scurl "http://www.amazon.com"
                               digest:sha256
                               pkey:rsa
                               (generate-key pkey:rsa 512)))
           (prog-list (add-fproxy-program prog-list 3s #:filter ".*third.*")))
      (check-equal? (url->string (run-fproxy-programs prog-list "http://www.amazon.com/first"))
                    "http://www.amazon.com/1"
                    "Giving a string to run-fproxy-programs did not act as expected.")
      (check-equal? (url->string (run-fproxy-programs prog-list (string->url "http://www.amazon.com/second")))
                    "http://www.amazon.com/2"
                    "Giving an url to run-fproxy-programs did not act as expected.")
      (check-equal? (run-fproxy-programs prog-list (generate-scurl "http://www.amazon.com/third/"
                                                                 digest:sha256
                                                                 pkey:rsa
                                                                 (generate-key pkey:rsa 1024)))
                    3s
                    "Giving a scurl to run-fproxy-programs did not act as expected.")
      (check-false (run-fproxy-programs prog-list (make-bytes 4))
                   "Should have returned false when handing run-fproxy-programs something other than an url, scurl or string."))
    
    ; Test that running multiple programs returns the expected result.
    (let* ((ae (generate-scurl "http://crazy.mort.fr/index.html"
                               digest:sha256
                               pkey:rsa
                               (generate-key pkey:rsa 256)))
           (prog-list (add-fproxy-program (new-program-list) ae
                                        #:filter ".*amazon.*"
                                        #:exclude ".*exclude.*"))
           
           (a (generate-scurl "http://crazy.mort.fr/index.html"
                              digest:sha256
                              pkey:rsa
                              (generate-key pkey:rsa 256)))
           (prog-list (add-fproxy-program prog-list a
                                        #:filter ".*amazon.*"))
           
           (ee (generate-scurl "http://crazy.mort.fr/index.html"
                               digest:sha256
                               pkey:rsa
                               (generate-key pkey:rsa 256)))
           (prog-list (add-fproxy-program prog-list ee
                                        #:filter ".*ebay.*"
                                        #:exclude ".*exclude.*"))
           
           (e (generate-scurl "http://crazy.mort.fr/index.html"
                              digest:sha256
                              pkey:rsa
                              (generate-key pkey:rsa 256)))
           (prog-list (add-fproxy-program prog-list e
                                        #:filter ".*ebay.*"))
           
           (pse (generate-scurl "http://crazy.mort.fr/index.html"
                                digest:sha256
                                pkey:rsa
                                (generate-key pkey:rsa 256)))
           (prog-list (add-fproxy-program prog-list pse
                                        #:prefix "prefix:"
                                        #:filter "^http://www.sf.*"
                                        #:exclude ".*exclude.*"))
           
           (ps (string->url "http://crazy.mort.fr/index.html"))
           (prog-list (add-fproxy-program prog-list ps
                                        #:prefix "prefix:"
                                        #:filter "^http://www.sf.*")))
      
      (check-equal? (run-fproxy-programs prog-list "http://www.amazon.com")
                    ae
                    "Failed to return the amazon exclude scurl.")
      (check-equal? (run-fproxy-programs prog-list "http://www.amazon.exclude.com")
                    a
                    "Failed to return the amazon scurl.")
      
      (check-equal? (run-fproxy-programs prog-list "http://www.ebay.com")
                    ee
                    "Failed to return the ebay exclude scurl.")
      (check-equal? (run-fproxy-programs prog-list "http://www.ebay.exclude.com")
                    e
                    "Failed to return the ebay scurl.")
      
      (check-equal? (run-fproxy-programs prog-list "prefix:http://www.sf.net")
                    pse
                    "Failed to return the sf exclude scurl.")
      (check-equal? (run-fproxy-programs prog-list "prefix:http://www.sf.exclude.net")
                    ps
                    "Failed to return the sf scurl.")
      (check-equal? (run-fproxy-programs prog-list "http://www.google.com")
                    #f
                    "Failed to return false on an url that should not have matched anything.")
     )
    )
   )
  )

; Provide everything
(provide (all-defined-out))

; Uncomment to run this test from this file.
;(require rackunit/text-ui)
;(run-tests forward-proxy-test)
