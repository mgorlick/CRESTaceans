#lang racket/base

(require rackunit

         "depends.rkt"
         "reverse-proxy.rkt"
         "program.rkt"
         
         "../peer-validation/scurl.rkt")

; Main reverse-proxy-test
(define reverse-proxy-test
  (test-suite
   "Tests for reverse-proxy-test.rkt"
   
   (test-case
    "Check the run-rproxy-programs function."
    
    ; Test that running multiple programs returns the expected result.
    (let* (; Add a program that is always run and returns not handled.
           (prog-list (add-rproxy-program (new-program-list) (lambda (msg) #f)))
           
           (ae (generate-scurl "http://www.amazon.com"
                               digest:sha256
                               pkey:rsa
                               (generate-key pkey:rsa 256)))
           (prog-list (add-rproxy-program prog-list (lambda (msg) ae)
                                        #:filter ".*amazon.*"
                                        #:exclude ".*exclude.*"))
           
           (a (generate-scurl "http://www.amazon.exclude.com"
                              digest:sha256
                              pkey:rsa
                              (generate-key pkey:rsa 256)))
           (prog-list (add-rproxy-program prog-list (lambda (msg) a)
                                        #:filter ".*amazon.*"))
           
           (ee (generate-scurl "http://www.ebay.com"
                               digest:sha256
                               pkey:rsa
                               (generate-key pkey:rsa 256)))
           (prog-list (add-rproxy-program prog-list (lambda (msg) ee)
                                        #:filter ".*ebay.*"
                                        #:exclude ".*exclude.*"))
           
           (e (generate-scurl "http://www.ebay.exclude.com"
                              digest:sha256
                              pkey:rsa
                              (generate-key pkey:rsa 256)))
           (prog-list (add-rproxy-program prog-list (lambda (msg) e)
                                        #:filter ".*ebay.*"))
           
           (pse (generate-scurl "http://prefix.sf.net"
                                digest:sha256
                                pkey:rsa
                                (generate-key pkey:rsa 256)))
           (prog-list (add-rproxy-program prog-list (lambda (msg) pse)
                                        #:prefix "http://prefix."
                                        #:filter "^sf.*"
                                        #:exclude ".*exclude.*"))
           
           (ps (generate-scurl "http://prefix.sf.exclude.net"
                               digest:sha256
                               pkey:rsa
                               (generate-key pkey:rsa 256)))
           (prog-list (add-rproxy-program prog-list (lambda (msg) ps)
                                        #:prefix "http://prefix."
                                        #:filter "^sf.*")))
      
      (check-equal? (run-rproxy-programs prog-list ae (list "header" "body"))
                    ae
                    "Failed to return the amazon exclude scurl.")
      (check-equal? (run-rproxy-programs prog-list a (list "header" "body"))
                    a
                    "Failed to return the amazon scurl.")
      
      (check-equal? (run-rproxy-programs prog-list ee (list "header" "body"))
                    ee
                    "Failed to return the ebay exclude scurl.")
      (check-equal? (run-rproxy-programs prog-list e (list "header" "body"))
                    e
                    "Failed to return the ebay scurl.")
      
      (check-equal? (run-rproxy-programs prog-list pse (list "header" "body"))
                    pse
                    "Failed to return the sf exclude scurl.")
      (check-equal? (run-rproxy-programs prog-list ps (list "header" "body"))
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
;(run-tests reverse-proxy-test)
