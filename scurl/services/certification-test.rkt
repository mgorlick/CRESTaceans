#lang racket/base

(require rackunit

         "depends.rkt"
         "certification.rkt"
         "program.rkt"
         
         "../peer-validation/scurl.rkt")

(define ret-scurl (generate-scurl "http://crazy.mort.fr/index.html"
                                  digest:sha256
                                  pkey:rsa
                                  (generate-key pkey:rsa 1024)))

; Main certification-test
(define certification-test
  (test-suite
   "Tests for certification-test.rkt"
   
   (test-case
    "Check the run-cert-programs function."
    
    ; Test that string, url, scurl and others are handled correctly.
    (let ((prog-list (add-cert-program (new-program-list) (lambda (name) (when (regexp-match? #rx"^http://www.amazon.com" name) ret-scurl)))))
      (check-equal? (run-cert-programs prog-list "http://www.amazon.com")
                    ret-scurl
                    "Giving a string to run-cert-programs did not act as expected.")
      (check-equal? (run-cert-programs prog-list (string->url "http://www.amazon.com"))
                    ret-scurl
                    "Giving an url to run-cert-programs did not act as expected.")
      (check-equal? (run-cert-programs prog-list (generate-scurl "http://www.amazon.com"
                                                                 digest:sha256
                                                                 pkey:rsa
                                                                 (generate-key pkey:rsa 1024)))
                    ret-scurl
                    "Giving a scurl to run-cert-programs did not act as expected.")
      (check-false (run-cert-programs prog-list (make-bytes 4))
                   "Should have returned false when handing run-cert-programs something other than an url, scurl or string."))
    
    ; Test that the correct argument is being passed to the programs
    (let ((prog-list (add-cert-program (new-program-list)
                                       (lambda (name) (when (string=? "http://www.amazon.com" name) ret-scurl))
                                       #:prefix "prefix:")))
      (check-equal? (run-cert-programs prog-list "prefix:http://www.amazon.com")
                    ret-scurl
                    "The prefix was not stripped correctly."))
    
    ; Test that running multiple programs returns the expected result.
    (let* ((ae (generate-scurl "http://crazy.mort.fr/index.html"
                               digest:sha256
                               pkey:rsa
                               (generate-key pkey:rsa 256)))
           (prog-list (add-cert-program (new-program-list) (lambda (name) ae)
                                        #:filter ".*amazon.*"
                                        #:exclude ".*exclude.*"))
           
           (a (generate-scurl "http://crazy.mort.fr/index.html"
                              digest:sha256
                              pkey:rsa
                              (generate-key pkey:rsa 256)))
           (prog-list (add-cert-program prog-list (lambda (name) a)
                                        #:filter ".*amazon.*"))
           
           (ee (generate-scurl "http://crazy.mort.fr/index.html"
                               digest:sha256
                               pkey:rsa
                               (generate-key pkey:rsa 256)))
           (prog-list (add-cert-program prog-list (lambda (name) ee)
                                        #:filter ".*ebay.*"
                                        #:exclude ".*exclude.*"))
           
           (e (generate-scurl "http://crazy.mort.fr/index.html"
                              digest:sha256
                              pkey:rsa
                              (generate-key pkey:rsa 256)))
           (prog-list (add-cert-program prog-list (lambda (name) e)
                                        #:filter ".*ebay.*"))
           
           (pse (generate-scurl "http://crazy.mort.fr/index.html"
                                digest:sha256
                                pkey:rsa
                                (generate-key pkey:rsa 256)))
           (prog-list (add-cert-program prog-list (lambda (name) pse)
                                        #:prefix "prefix:"
                                        #:filter "^http://www.sf.*"
                                        #:exclude ".*exclude.*"))
           
           (ps (generate-scurl "http://crazy.mort.fr/index.html"
                               digest:sha256
                               pkey:rsa
                               (generate-key pkey:rsa 256)))
           (prog-list (add-cert-program prog-list (lambda (name) ps)
                                        #:prefix "prefix:"
                                        #:filter "^http://www.sf.*")))
      
      (check-equal? (run-cert-programs prog-list "http://www.amazon.com")
                    ae
                    "Failed to return the amazon exclude scurl.")
      (check-equal? (run-cert-programs prog-list "http://www.amazon.exclude.com")
                    a
                    "Failed to return the amazon scurl.")
      
      (check-equal? (run-cert-programs prog-list "http://www.ebay.com")
                    ee
                    "Failed to return the ebay exclude scurl.")
      (check-equal? (run-cert-programs prog-list "http://www.ebay.exclude.com")
                    e
                    "Failed to return the ebay scurl.")
      
      (check-equal? (run-cert-programs prog-list "prefix:http://www.sf.net")
                    pse
                    "Failed to return the sf exclude scurl.")
      (check-equal? (run-cert-programs prog-list "prefix:http://www.sf.exclude.net")
                    ps
                    "Failed to return the sf scurl.")
     )
    )
   )
  )

; Provide everything
(provide (all-defined-out))

; Uncomment to run this test from this file.
;(require rackunit/text-ui)
;(run-tests certification-test)
