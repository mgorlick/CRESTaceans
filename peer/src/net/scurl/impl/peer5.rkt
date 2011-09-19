#lang racket/base

(require racket/contract racket/list)

; Peer 5

(require "../peer-validation/depends.rkt"
         "../peer-validation/scurl.rkt"
         "../peer-validation/host-id.rkt"
         "../common/logging-utils.rkt"
         "../services/revocation.rkt"
         "../services/forward-proxy.rkt"
         "../services/program.rkt"
         "../services/certification.rkt"
         "../services/reverse-proxy.rkt"
         "generate-test-scurls.rkt"
         "peer-validation-impl.rkt")

(define (run)
  (create-missing-scurl-file)
  
  (let-values (((self-scurl scurls revo-certs) (read-peer-file 4)))
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
           
           ; Setup revocation programs, block nothing for peer4.
           (revo-progs (new-program-list))
           
           ; Setup the forward proxy programs, not doing any for peer4.
           (fproxy-progs (new-program-list))
           
           ; Setup the reverse proxy programs, not doing any for peer4.
           (rproxy-progs (new-program-list)))
      (run-peer self-scurl cert-progs revo-progs fproxy-progs rproxy-progs))))
