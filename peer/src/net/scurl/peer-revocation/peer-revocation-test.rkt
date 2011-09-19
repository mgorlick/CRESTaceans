#lang racket/base

(require racket/contract racket/list)

(require rackunit
         "depends.rkt"
         "peer-revocation.rkt"
         
         "../peer-validation/scurl.rkt"
         "../peer-validation/scurl-test.rkt"
         "../peer-validation/peer-validation-msgs.rkt"
         "../peer-validation/peer-validation.rkt")

; State variables for testing peer to peer verification
(define (pass-revocation peer1-scurl peer2-scurl)
  ; Create the communication ports as pipes.
  (let-values (((peer1-in peer1-out) (make-pipe))
               ((peer2-in peer2-out) (make-pipe)))
    (let ((did-pass #f))
      (let ((t (thread (lambda ()
                         (set! did-pass (handle-server-revocation-response peer2-scurl peer1-in peer2-out))
                         ; Close the output port to signify that we are done.
                         (close-output-port peer2-out))))
            (scurl (handle-client-authentication peer1-scurl peer2-scurl peer2-in peer1-out)))
        
        ; Close the output port to signify that we are done.
        (close-output-port peer1-out)
        
        ; Wait for the thread to finish.
        (when (thread-running? t)
          (thread-wait t))
        
        (close-input-port peer1-in)
        (close-input-port peer2-in)
        
        (if (and
             (not (scurl? scurl))
             (equal? did-pass #t))
            #t
            #f)))))

(define (fail-revocation peer1-scurl peer2-scurl)
  ; Create the communication ports as pipes.
  (let-values (((peer1-in peer1-out) (make-pipe))
               ((peer2-in peer2-out) (make-pipe)))
    (let ((response-scurl #f))
      (let ((t (thread (lambda ()
                         (set! response-scurl (handle-server-revocation-response peer2-scurl peer1-in peer2-out))
                         ; Close the output port to signify that we are done.
                         (close-output-port peer2-out)))))
        
        ; Write some bogus data.
        (write-bytes (make-bytes 55) peer1-out)
        (flush-output peer1-out)
        
        ; Close the output port to signify that we are done.
        (close-output-port peer1-out)
        
        ; Wait for the thread to finish.
        (when (thread-running? t)
          (thread-wait t))

        (close-input-port peer1-in)
        (close-input-port peer2-in)

        (if (scurl? response-scurl)
            #t
            #f)))))

(define peer-revocation-test
  (test-suite
   "Tests for peer-revocation.rkt"
   
   (test-case
    "Testing the revocation protocol."
    
    ; Create the scurls to use for authentication.
    (let ((peer1-scurl (generate-scurl "http://www.amazon.com:3456/index.html"
                                       digest:sha256
                                       pkey:rsa
                                       (generate-key pkey:rsa 1024)))
          (peer2-scurl (generate-scurl "http://www.ebay.com:3456/index.html"
                                       digest:sha256
                                       pkey:rsa
                                       (generate-key pkey:rsa 512)))
          (peer3-scurl (generate-scurl "http://www.target.com"
                                       digest:sha512
                                       pkey:rsa
                                       (generate-key pkey:rsa 2056))))
      (check-true (pass-revocation peer1-scurl peer2-scurl)
                  "The two peers failed to complete revocation nicely.")
      (check-false (fail-revocation peer1-scurl peer2-scurl)
                   "The two peers passed revocation nicely."))
    )
   ))

; Provide everything.
(provide (all-defined-out))

; Uncomment to run this test from this file.
;(require rackunit/text-ui)
;(run-tests peer-revocation-test)
