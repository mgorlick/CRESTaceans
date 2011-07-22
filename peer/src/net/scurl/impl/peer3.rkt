#lang racket/base

(require racket/contract racket/list)

; Peer 3

(require "../peer-validation/depends.rkt"
         "../peer-validation/scurl.rkt"
         "../peer-validation/host-id.rkt"
         "../services/revocation.rkt"
         "../services/program.rkt"
         "../services/certification.rkt"
         "../services/reverse-proxy.rkt"
         "generate-test-scurls.rkt"
         "peer-validation-impl.rkt")

(define (rproxy-service self-scurl peer-scurl p4-list-in p4-list-out p3-list-in p3-list-out)
  ; Read the message from p3-list-in and write it to p4-list-out.
  (let ((msg-list (read p3-list-in)))
    (write msg-list p4-list-out)
    (flush-output p4-list-out)
    
    ; Read the echo back from p4-list-in and write it to p3-list-out.
    (let ((echo-msg-list (read p4-list-in)))
      (write echo-msg-list p3-list-out)
      (flush-output p3-list-out)
  
      ; Do it again if the ports are open
      (unless (or
               (eof-object? msg-list)
               (eof-object? echo-msg-list))
        (rproxy-service self-scurl peer-scurl p4-list-in p4-list-out p3-list-in p3-list-out)))))

(define (run)
  (create-missing-scurl-file)
  
  (let-values (((self-scurl scurls revo-certs) (read-peer-file 2)))
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
            ; Block the access to peer4 using it's revo-cert.
            (add-revo-program (new-program-list)
                              (lambda (host-id)
                                (when (string=?
                                       host-id
                                       (host-id-bytes->host-id-string (scurl-host-id (fourth scurls))))
                                  (fourth revo-certs)))))
           (revo-progs
            ; Block anything that has bad in the url using host-id blocking.
            (add-revo-program revo-progs
                              (lambda (host-id) #t)
                              #:block #t
                              #:filter ".*bad.*"
                              ))
           
           ; Setup the forward proxy programs, not doing any for peer3.
           (fproxy-progs (new-program-list))
           
           ; Setup the reverse proxy programs, anyone connecting from one to three
           ; will be forwarded to peer4.
           (rproxy-progs
            ; When peer one connects let it be handled by the rproxy service.
            (add-rproxy-program (new-program-list)
                                (lambda (msg)
                                  ; The received message in our test framework is the input/output ports to talk
                                  ; to the client peer.
                                  
                                  ; Connect to peer 5 and pipe all requests/response between the two peers.
                                  (handle-local-request
                                   "5"
                                   self-scurl
                                   cert-progs
                                   revo-progs
                                   fproxy-progs
                                   ; This is the service that handle-local-request will call
                                   (lambda (self-scurl peer-scurl list-in list-out)
                                     ; Call the rproxy-service define above which will pipe messages between the
                                     ; two peers.
                                     (rproxy-service self-scurl peer-scurl list-in list-out (first msg) (second msg))))
                                  ; Return true to indicate that we handled the msg.
                                  #t)
                                #:filter ".*one.*")))
      (run-peer self-scurl cert-progs revo-progs fproxy-progs rproxy-progs))))
