#lang racket/base

(require racket/contract racket/list)

; Peer 2

(require "../peer-validation/depends.rkt"
         "../peer-validation/scurl.rkt"
         "../services/revocation.rkt"
         "../services/forward-proxy.rkt"
         "../services/program.rkt"
         "../services/certification.rkt"
         "generate-test-scurls.rkt"
         "peer-validation-impl.rkt")

(define (run)
  (create-missing-scurl-file)
  
  (let-values (((self-scurl scurls revo-certs) (read-peer-file 1)))
    (let* (
           ; Setup certification programs.
           (cert-progs
            ; Convert anything this is a single index into a scurl indexed from the
            ; known scurl list.
            (add-cert-program (new-program-list)
                              (lambda (name)
                                (let* ((number (string->number name))
                                       (number (if (exact-integer? number) (sub1 number) number)))
                                  (when (and
                                         (exact-integer? number)
                                         (>= number 0)
                                         (< number (length scurls)))
                                    (list-ref scurls number))))
                              ))
           (cert-progs
            ; Convert anything that matches the non-scurl form into the scurl form.
            (add-cert-program cert-progs
                              (lambda (url)
                                (ormap (lambda (scurl) 
                                         (when (string=?
                                                (url->string (scurl->url-without-host-id scurl))
                                                url)
                                           scurl))
                                       scurls))))
           
           ; Setup revocation programs.
           (revo-progs
            ; Block the access to peer3 using host-id blocking
            (add-revo-program (new-program-list)
                              (lambda (host-id) #t)
                              #:block #t
                              #:filter (scurl->string (third scurls))))
           (revo-progs
            ; Block anything that has bad in the url using host-id blocking.
            (add-revo-program revo-progs
                              (lambda (host-id) #t)
                              #:block #t
                              #:filter ".*bad.*"
                              ))
           
           ; Setup the forward proxy programs
           (fproxy-progs
            ; Send anything trying to connect to peer5 to peer4
            (add-fproxy-program (new-program-list)
                                (fourth scurls)
                                #:filter (scurl->string (fifth scurls))))
           
           ; Setup the reverse proxy programs, not doing any for peer2.
           (rproxy-progs (new-program-list)))
      (run-peer self-scurl cert-progs revo-progs fproxy-progs rproxy-progs))))
