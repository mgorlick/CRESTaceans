#lang racket/base

(require racket/contract racket/list)

(require rackunit
         racket/serialize
         
         "depends.rkt"
         "crypto.rkt"
         "peer-secure-session.rkt"
         
         "../peer-validation/scurl.rkt")

; Utility to convert a list of primitives to a byte-string.
(define (list->bytes lst in out)
  ; Write the serialized list to the output.
  (write (serialize lst) out)
  (flush-output out)
  
  ; Read and return bytes
  (read-bytes (pipe-content-length in) in))

; Utility to convert a byte-string to a list of primitives.
(define (bytes->list data in out)
  ; Write the bytes to the output.
  (write-bytes data out)
  (flush-output out)
  
  ; Read and deserialize then return.
  (deserialize (read in)))

; State variables for testing peer to peer verification
(define (validate-peers peer1-scurl peer2-scurl)
  ; Create the communication ports as pipes.
  (letrec-values (((peer1-in peer1-out) (make-pipe))
                  ((peer2-in peer2-out) (make-pipe))
                  ((bin bout) (make-pipe))
                  ((l->b) (values
                           (lambda (data)
                             (list->bytes data bin bout))))
                  ((b->l) (values
                           (lambda (data)
                             (bytes->list data bin bout)))))
    (let ((t (thread (lambda ()
                      (handle-peer-secure-response peer2-scurl peer1-scurl peer1-in peer2-out l->b b->l)))))
      (let ((sess (handle-peer-secure-request peer1-scurl peer2-scurl peer2-in peer1-out l->b b->l)))
        
        (close-input-port peer1-in)
        (close-output-port peer1-out)
      
        (close-input-port peer2-in)
        (close-output-port peer2-out)
      
        (if (session? sess)
            #t
            #f)))))

(define (fail-validate-peers peer1-scurl peer2-scurl peer3-scurl)
  ; Create the communication ports as pipes.
  (letrec-values (((peer1-in peer1-out) (make-pipe))
                  ((peer2-in peer2-out) (make-pipe))
                  ((bin bout) (make-pipe))
                  ((l->b) (values
                           (lambda (data)
                             (list->bytes data bin bout))))
                  ((b->l) (values
                           (lambda (data)
                             (bytes->list data bin bout)))))
    (let ((t (thread (lambda ()
                       (handle-peer-secure-response peer2-scurl peer1-scurl peer1-in peer2-out l->b b->l)))))
      (let ((sess (handle-peer-secure-request peer3-scurl peer2-scurl peer2-in peer1-out l->b b->l)))
        
        (close-input-port peer1-in)
        (close-output-port peer1-out)
        
        (close-input-port peer2-in)
        (close-output-port peer2-out)

        (if (session? sess)
            #t
            #f)))))

; Main test
(define peer-secure-session-test
  (test-suite
   "Tests for peer-secure-session.rkt"
   
   (test-case
    "Testing the secure session protocol."
    
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

      (check-true (validate-peers peer1-scurl peer2-scurl)
                  "The two peers failed to validated.")
      (check-false (fail-validate-peers peer1-scurl peer2-scurl peer3-scurl)
                   "The two peers passed when they should have failed."))
    )
   ))

; Provide everything.
(provide (all-defined-out))

; Uncomment to run this test from this file.
;(require rackunit/text-ui)
;(run-tests peer-secure-session-test)
