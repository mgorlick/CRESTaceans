#lang racket/base

(require racket/contract racket/list)

; Peer 1

(require "../peer-validation/depends.rkt"
         "../peer-validation/scurl.rkt"
         "../peer-validation/host-id.rkt"
         "../services/revocation.rkt"
         "../services/program.rkt"
         "../services/certification.rkt"
         "generate-test-scurls.rkt"
         "peer-validation-impl.rkt")

(define (run-client-single self-scurl peer-scurl)
  (handle-outgoing-request self-scurl peer-scurl (lambda (self-scurl peer-scurl in out) #t)))

(define (run-client num)
  (create-missing-scurl-file)
  (let-values (((self-scurl scurls revo-certs) (read-peer-file 0)))
  
    (let ((start (current-milliseconds)))
      (for-each (lambda (arg)
                  (run-client-single self-scurl (second scurls)))
                (build-list num values))
      (let* ((stop (current-milliseconds))
             (delta (- stop start)))
        (printf "Start: ~a Stop: ~a Delta: ~a\n" start stop delta)))))

(define (run-server)
  (create-missing-scurl-file)
  
  (let ((start (current-milliseconds)))
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
             
             ; Setup revocation programs.
             (revo-progs (new-program-list))
             
             ; Setup the forward proxy programs, not doing any for peer1.
             (fproxy-progs (new-program-list))
             
             ; Setup the reverse proxy programs, not doing any for peer1.
             (rproxy-progs (new-program-list)))
        (run-peer self-scurl cert-progs revo-progs fproxy-progs rproxy-progs)
        
        (let* ((stop (current-milliseconds))
               (delta (- stop start)))
          (printf "Start: ~a Stop: ~a Delta: ~a\n" start stop delta))))))
