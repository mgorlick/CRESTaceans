#lang racket/base

(require rackunit

         "depends.rkt"
         "revocation.rkt"
         "program.rkt"
         
         "../peer-revocation/revo-cert.rkt"
         "../peer-validation/scurl.rkt")

; Main revocation-test
(define revocation-test
  (test-suite
   "Tests for revocation-test.rkt"
   
   (test-case
    "Check the run-revo-prog-list function."
    
    ; Test that running multiple programs returns the expected result.
    (let* ((ae (generate-scurl "http://www.amazon.com"
                               digest:sha256
                               pkey:rsa
                               (generate-key pkey:rsa 512)))
           (aerc (scurl->revo-cert ae))
           (prog-list (add-revo-program (new-program-list) (lambda (name) aerc)
                                        #:filter ".*amazon.*"
                                        #:exclude ".*exclude.*"))
           
           (ab (generate-scurl "http://www.amazon.block.exclude.com"
                               digest:sha256
                               pkey:rsa
                               (generate-key pkey:rsa 512)))
           (prog-list (add-revo-program prog-list (lambda (name) #f)
                                        #:block #t
                                        #:filter ".*amazon.*block.*"))
           
           (ee (generate-scurl "http://www.ebay.com"
                               digest:sha256
                               pkey:rsa
                               (generate-key pkey:rsa 512)))
           (eerc (scurl->revo-cert ee))
           (prog-list (add-revo-program prog-list (lambda (host-id) eerc)
                                        #:filter ".*ebay.*"
                                        #:exclude ".*exclude.*"))
           
           (eb (generate-scurl "http://www.ebay.block.exclude.com"
                               digest:sha256
                               pkey:rsa
                               (generate-key pkey:rsa 512)))
           (prog-list (add-revo-program prog-list (lambda (host-id) #t)
                                        #:block #t
                                        #:filter ".*ebay.*block.*"))
           
           (sf (generate-scurl "http://www.sf.net"
                               digest:sha256
                               pkey:rsa
                               (generate-key pkey:rsa 512)))
           (prog-list (add-revo-program prog-list (lambda (host-id) eerc)
                                        #:filter "^http://www.sf.*"))
           
           (exception (generate-scurl "http://www.any.org/exception"
                                      digest:sha256
                                      pkey:rsa
                                      (generate-key pkey:rsa 512)))
           (prog-list (add-revo-program prog-list (lambda (host-id) (+ "Hello" (make-bytes 4)))
                                        #:filter ".*exception")))
      
      (check-equal? (run-revo-programs prog-list ae)
                    aerc
                    "Failed to return the amazon exclude revo-cert.")
      (check-equal? (run-revo-programs prog-list ab)
                    #t
                    "Failed to return true for host-id blocking.")
      
      (check-equal? (run-revo-programs prog-list ee)
                    eerc
                    "Failed to return the ebay exclude revo-cert.")
      (check-equal? (run-revo-programs prog-list eb)
                    #t
                    "Failed to return true for host-id blocking.")
      
      (check-equal? (run-revo-programs prog-list sf)
                    #f
                    "Failed to return false when an invalid revo-cert is returned.")
      
      (check-equal? (run-revo-programs prog-list exception)
                    #f
                    "Failed to return false when an exception is returned.")
      
      (check-equal? (run-revo-programs prog-list (string->url "http://www.ebay.block.exclude.com"))
                    #t
                    "Failed to return true for host-id blocking when passing an url."))
    )
   )
  )

; Provide everything
(provide (all-defined-out))

; Uncomment to run this test from this file.
;(require rackunit/text-ui)
;(run-tests revocation-test)
