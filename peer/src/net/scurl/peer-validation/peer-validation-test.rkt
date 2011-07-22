#lang racket/base

(require racket/contract racket/list)

(require rackunit
         "depends.rkt"
         "scurl.rkt"
         "scurl-utils.rkt"
         "scurl-test.rkt"
         "peer-validation-msgs.rkt"
         "peer-validation.rkt")

; State variables for testing peer to peer verification
(define (authenticate-peers peer1-scurl peer2-scurl)
  ; Create the communication ports as pipes.
  (let-values (((peer1-in peer1-out) (make-pipe))
               ((peer2-in peer2-out) (make-pipe)))
    (let ((handle-revocation (lambda (s) #f))
          (response-scurl #f))
      (let ((t (thread (lambda ()
                         (set! response-scurl (handle-server-authentication peer2-scurl handle-revocation peer1-in peer2-out))
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
             (scurl? scurl)
             (scurl? response-scurl))
            #t
            #f)))))

(define (fail-authenticate-peers peer1-scurl peer2-scurl peer3-scurl)
  ; Create the communication ports as pipes.
  (let-values (((peer1-in peer1-out) (make-pipe))
               ((peer2-in peer2-out) (make-pipe)))
    (let ((handle-revocation (lambda (s) #f))
          (response-scurl #f))
      (let ((t (thread (lambda ()
                         (set! response-scurl (handle-server-authentication peer2-scurl handle-revocation peer1-in peer2-out))
                         ; Close the output port to signify that we are done.
                         (close-output-port peer2-out))))
            (scurl (handle-client-authentication peer2-scurl peer3-scurl peer2-in peer1-out)))
        
        ; Close the output port to signify that we are done.
        (close-output-port peer1-out)
        
        ; Wait for the thread to finish.
        (when (thread-running? t)
          (thread-wait t))

        (close-input-port peer1-in)
        (close-input-port peer2-in)

        (if (and
             (scurl? scurl)
             (scurl? response-scurl))
            #t
            #f)))))

; Main crypto test
(define peer-validation-test
  (test-suite
   "Tests for peer-validation.rkt"
   
   ; Test utility functions
   (test-case
    "Testing utility functions."
    (check-true (= (bytes-length (generate-nonce 55)) 55)
                "The length of the generated nonce was not as expected.")
    
    ; Make sure that the verify-signature method works for a signature created
    ; by the create-signature method.
    (let ((time (current-seconds))
          (nonce (generate-nonce 28)))
      (check-true (verify-signature scurl3-full time nonce (create-signature scurl3-full time nonce))
                  "Failed to verify a signature that should have been correct.")
      (check-false (verify-signature scurl3-full time nonce (create-signature scurl3-full (+ time 5) nonce))
                   "Verified a signature when the timestamp was different.")
      (check-false (verify-signature scurl3-full time nonce (create-signature scurl3-full time (generate-nonce 28)))
                   "Verified a signature when the nonce was different.")
      (check-false (verify-signature scurl3-full time nonce (create-signature scurl4-full time nonce))
                   "Verified a signature when the scurl was different.")
      (check-false (verify-signature scurl3-full time nonce (create-signature scurl4-full (+ time 54) (generate-nonce 28)))
                   "Verified a signature when everything was different."))
    )
   
   ; Test authentication functions
   (test-case
    "Testing the authentication message handling."
    (let* ((cam1-time (current-seconds))
           (cam1-nonce (generate-nonce 28))
           (cam1 (client-auth-msg scurl3-full cam1-time cam1-nonce (create-signature scurl3-full cam1-time cam1-nonce)))
           (cam2-time (current-seconds))
           (cam2-nonce (generate-nonce 28))
           (cam2 (client-auth-msg scurl4-full cam2-time cam2-nonce (create-signature scurl4-full cam2-time cam2-nonce)))
           
           (sam1-time (current-seconds))
           (sam1-nonce (generate-nonce 28))
           (sam1 (server-auth-msg scurl3-full (create-signature scurl3-full sam1-time sam1-nonce))))
                             
      (check-true (authenticate-client-auth-msg? cam1-time cam1-nonce cam1)
                  "Failed to validate a correct client-auth-msg.")
      (check-false (authenticate-client-auth-msg? cam1-time cam1-nonce cam2)
                   "Passed a validation for an invalid client-auth-msg that should fail.")
      
      (check-true (authenticate-server-auth-msg? scurl3-full sam1-time sam1-nonce sam1)
                  "Failed a validation for a valid server-auth-msg.")
      
      (check-false (authenticate-server-auth-msg? scurl4-full sam1-time sam1-nonce sam1)
                   "Passed a validation for a server-auth-msg that should fail."))
    )
   
   (test-case
    "Testing the validation protocol."
    
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
      (check-true (authenticate-peers peer1-scurl peer2-scurl)
                  "The two peers failed to validated.")
      (check-false (fail-authenticate-peers peer1-scurl peer2-scurl peer3-scurl)
                   "The two peers passed when they should have failed."))
    )
   ))

; Provide everything.
(provide (all-defined-out))

; Uncomment to run this test from this file.
;(require rackunit/text-ui)
;(run-tests peer-validation-test)