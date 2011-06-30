#lang racket

(require rackunit
         
         "program.rkt"
         "../peer-validation/scurl-test.rkt")

; Define a set of testing values.
(define cp1 (program 1 #f #f #f (lambda () #f)))
(define cp2 (program 2 #f #f #f (lambda () #f)))

; Main revocation-test
(define program-test
  (test-suite
   "Tests for program-test.rkt"
   
   (test-case
    "Check the program and program-list functions."
    
    (check-true (program? cp1)
                "Failed to create a new revocation program.")
    (check-true (program-list? (new-program-list))
                "Failed to create a new revocation program list.")
    (check-true (program=? cp1 cp1)
                "Failed to validate that the same programram returns true from program=?")
    (check-false (program=? cp1 cp2)
                 "Failed to invalidate two different programs when given to program=?")
    
    (letrec ((proc (lambda () #f))
             (rp3a (program 3 "prefix" (regexp "filter") (regexp "exclude") proc))
             (rp3b (program 3 "prefix" (regexp "filter") (regexp "exclude") proc)))
      (check-false (program=? rp3a rp3b)
                   "Successfully validated two programs that are not the same."))
    (letrec ((proc (lambda () #f))
             (prefix "prefix")
             (filter (regexp "filter"))
             (exclude (regexp "exclude"))
             (rp4a (program 4 prefix filter exclude proc))
             (rp4b (program 4 prefix filter exclude proc)))
      (check-true (program=? rp4a rp4b)
                  "Failed to validate two programs that are the same."))
    )
   
   (test-case
    "Check the add and remove functions for program-lists."
    
    (check-true (program-list? (add-program (new-program-list) (lambda () "Hello")))
                "Failed to return a program-list when calling the add-program function with only required arguments.")
    (check-true (program-list? (add-program (new-program-list)
                                            (lambda () "Hello")
                                            #:prefix ""
                                            #:filter ""
                                            #:exclude ""))
                "Failed to return a program-list when calling the add-program function with all arguments.")
    (check-true (program=?
                 cp1
                 (car
                  (program-list-programs
                   (add-program (new-program-list)
                                (program-proc cp1)))))
                "The add-program did not add the expected program to the returned program-list.")
    (check-true (=
                 0
                 (length
                  (program-list-programs
                   (remove-program 1 (add-program (new-program-list) (lambda () "Hello"))))))
                "The program was not correctly removed from the program-list.")
    (check-true (=
                 2
                 (length
                  (program-list-programs
                   (remove-program
                    2
                    (add-program
                     (add-program (add-program (new-program-list)
                                               (lambda () "Hello"))
                                  (lambda () "Hello"))
                     (lambda () "Hello"))))))
                "The program was not correctly removed from the program-list.")
    )
   
   (test-case
    "Check the match functions."
    
    (let ((rp3 (car (program-list-programs
                     (add-program (new-program-list)
                                  (lambda () #t)
                                  #:prefix "http://www.amazon"
                                  #:filter ".*amazon.*"
                                  #:exclude ".*ebay.*")))))
      
      (check-true (match-prefix? "http://www.amazon.com" rp3)
                  "Failed to match a valid prefix.")
      (check-false (match-prefix? "http://www.ebay.com" rp3)
                   "Succesfully matched an invalid prefix.")
      
      (check-equal? (strip-prefix "http://www.amazon.com" rp3)
                    ".com"
                    "Failed to return the expected string when stripping the prefix.")
      (check-equal? (strip-prefix "http://www.amazon.com/index.html" rp3)
                    ".com/index.html"
                    "Failed to return the expected string when stripping the prefix.")
      (check-equal? (strip-prefix "http://www.ama" rp3)
                    ""
                    "Failed to return the empty string when stripping a prefix whose string is too short.")
      
      (check-true (match-filter? "http://www.amazon.com" rp3)
                  "Failed to match a valid filter.")
      (check-false (match-filter? "http://www.ebay.com" rp3)
                   "Succesfully matched an invalid filter.")
      
      (check-true (match-exclude? "http://www.ebay.com" rp3)
                  "Failed to match a valid exclude.")
      (check-false (match-exclude? "http://www.amazon.com" rp3)
                   "Succesfully matched an invalid exclude."))
    )
   
   (test-case
    "Check the run-program function."
    
    (let (; Setup all combinations of prefix, filter and exclude.
          (n-n-n (program 1 #f #f #f (lambda (host-id) #t)))
          (n-n-e (program 1 #f #f (regexp ".*exclude.*") (lambda (host-id) #t)))
          (n-f-n (program 1 #f (regexp ".*filter.*") #f (lambda (host-id) #t)))
          (n-f-e (program 1 #f (regexp ".*filter.*") (regexp ".*exclude.*") (lambda (host-id) #t)))
          (p-n-n (program 1 "prefix" #f #f (lambda (host-id) #t)))
          (p-n-e (program 1 "prefix" #f (regexp ".*exclude.*") (lambda (host-id) #t)))
          (p-f-n (program 1 "prefix" (regexp ".*filter.*") #f (lambda (host-id) #t)))
          (p-f-e (program 1 "prefix" (regexp ".*filter.*") (regexp ".*exclude.*") (lambda (host-id) #t)))
          
          ; Setup all types of procedures to test, exceptions and non procedures.
          (rp-exception (program 1 #f #f #f (lambda (host-id) (+ "Bad number" (make-bytes 5)))))
          (rp-arity-exception (program 1 #f #f #f (lambda () #t)))
          
          (rp-proc (program 1 #f #f #f "OUTPUT"))
          (rp-not-proc (program 1 #f #f #f "NOT A PROCEDURE"))
          (arg "host-id"))
      
      (check-true (last (run-program n-n-n "http://www.any.org" arg))
                  "n-n-n: Failed to run the program when prefix, filter and exclude were disabled.")
      
      (check-true (last (run-program n-n-e "http://www.any.org" arg))
                  "n-n-e: Program was excluded when it should not have been.")
      (check-equal? (last (run-program n-n-e "http://www.exclude.org" arg))
                    'not-run
                    "n-n-e: Program was not excluded when it should have been.")
      
      (check-true (last (run-program n-f-n "http://www.filter.org" arg))
                  "n-f-n: Program did not pass the filter when it should have.")
      (check-equal? (last (run-program n-f-n "http://www.any.org" arg))
                    'not-run
                    "n-f-n: Program passed the filter when it should not have.")
      
      (check-true (last (run-program n-f-e "http://www.filter.org" arg))
                  "n-f-e: Program should have passed the filter and exclude and did not.")
      (check-equal? (last (run-program n-f-e "http://www.any.org" arg))
                    'not-run
                    "n-f-e: Program should not have passed the filter and did.")
      (check-equal? (last (run-program n-f-e "http://www.filter.exclude.org" arg))
                    'not-run
                    "n-f-e: Program should have passed the filter, but not the exclude, yet did.")
      
      (check-true (last (run-program p-n-n "prefix:http://www.any.org" arg))
                  "p-n-n: Program should have passed the filter.")
      (check-equal? (last (run-program p-n-n "http://www.any.org" arg))
                    'not-run
                    "p-n-n: Program should have failed to pass the filter.")
      
      (check-true (last (run-program p-n-e "prefix:http://www.any.org" arg))
                  "p-n-e: Program should have passed the prefix and exclude.")
      (check-equal? (last (run-program p-n-e "prefix:http://www.exclude.org" arg))
                    'not-run
                    "p-n-e: Program should have passed the prefix, but not the exclude.")
      
      (check-true (last (run-program p-f-n "prefix:http://www.filter.org" arg))
                  "p-f-n: Program should have passed the prefix and the filter.")
      (check-equal? (last (run-program p-f-n "prefix:http://www.any.org" arg))
                    'not-run
                    "p-f-n: Program should have passed the prefix and not the filter.")
      
      (check-true (last (run-program p-f-e "prefix:http://www.filter.org" arg))
                  "p-f-e: Program should have passed the prefix, filter and exclude.")
      (check-equal? (last (run-program p-f-e "prefix:http://www.filter.exclude.org" arg))
                    'not-run
                    "p-f-e: Program should have passed the prefix, filter, but fail to pass exclude.")
      (check-equal? (last (run-program p-f-e "prefix:http://www.any.org" arg))
                    'not-run
                    "p-f-e: Program should have passed the prefix, but fail to pass the filter.")
      (check-equal? (last (run-program p-f-e "http://www.any.org" arg))
                    'not-run
                    "p-f-e: Program should have failed to pass the prefix and filter.")
      
      ; Check procedure tests.
      (check-true (exn? (last (run-program rp-exception "http://www.any.org" arg)))
                  "Should have failed because the procedure raises an exception.")
      (check-true (exn? (last (run-program rp-arity-exception "http://www.any.org" arg)))
                  "Should have failed because the procedure does not accept the correct number of arguments.")
      
      (check-equal? (last (run-program rp-proc "http://www.any.org" arg))
                    "OUTPUT"
                    "Failed to return the expected output when the procedure is run.")
      (check-equal? (last (run-program rp-not-proc "http://www.any.org" arg))
                    "NOT A PROCEDURE"
                    "Failed to return the expected output when the procedure is not a procedure.")
      )
    )
   )
  )

; Provide everything
(provide (all-defined-out))

; Uncomment to run this test from this file.
;(require rackunit/text-ui)
;(run-tests program-test)
