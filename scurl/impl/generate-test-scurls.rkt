#lang racket
  
  (require 
   "../peer-revocation/revo-cert.rkt"
   "../peer-validation/depends.rkt"
   "../peer-validation/scurl.rkt"
   "../peer-validation/scurl-utils.rkt")
  
  (provide (all-defined-out))
  
  ; Generate a list of random scurls.
  (define (generate-scurls)
    (list
     (generate-scurl "http://127.0.0.1:6001/one.html"
                     digest:sha256
                     pkey:rsa
                     (generate-key pkey:rsa 2056))
     (generate-scurl "http://127.0.0.1:6002/two.html"
                     digest:sha256
                     pkey:rsa
                     (generate-key pkey:rsa 1024))
     (generate-scurl "http://127.0.0.1:6003/three.html"
                     digest:sha256
                     pkey:rsa
                     (generate-key pkey:rsa 512))
     (generate-scurl "http://127.0.0.1:6004/four.html"
                     digest:sha256
                     pkey:rsa
                     (generate-key pkey:rsa 512))
     (generate-scurl "http://127.0.0.1:6005/five.html"
                     digest:sha256
                     pkey:rsa
                     (generate-key pkey:rsa 512))
     (generate-scurl "http://127.0.0.1:6006/six.html"
                     digest:sha256
                     pkey:rsa
                     (generate-key pkey:rsa 512))))
  
  (define (generate-revo-certs scurl-list)
    (map scurl->revo-cert scurl-list))
  
  (define (write-private-scurl-file)
    (letrec ((scurls (generate-scurls))
             (scurls-strings (map scurl->strings scurls)))
      (let ((file (open-output-file "/tmp/scurls" #:mode 'text #:exists 'replace)))
        (write scurls-strings file)
        (close-output-port file))))
      
  (define (read-private-scurl-file)
    (let ((file (open-input-file "/tmp/scurls" #:mode 'text)))
      (let ((scurls-strings (read file)))
        (close-input-port file)
        (map strings->scurl scurls-strings))))
  
  (define (read-peer-file index)
    (letrec ((scurls (read-private-scurl-file))
             (peer-scurl (list-ref scurls index)))
      (values
       peer-scurl
       (map scurl->public-scurl scurls)
       (map scurl->revo-cert scurls))))
  
  ; Creates a scurl file when it is not found.
  (define (create-missing-scurl-file)
    (when (not (file-exists? "/tmp/scurls"))
      (write-private-scurl-file)))
       
  
  ; Use the (write-private-scurl-file) function to generate all scurls in a
  ; file in /tmp/scurls.  Then when running peer1.rkt they will ask for the
  ; correct information.

