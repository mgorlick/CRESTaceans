#lang racket/base

(require racket/contract racket/list)
  
  (require "../peer-validation/depends.rkt"
           "../peer-validation/scurl.rkt"
           "../peer-secure-session/crypto.rkt"
           "../peer-secure-session/peer-secure-session.rkt"
           "../services/program.rkt"
           "../services/certification.rkt"
           "generate-test-scurls.rkt"
           "peer-validation-impl.rkt"
           "peer-utils.rkt")
  
  ; Create the logger for the peer-validation-impl module.
  (define logger (make-logger 'peer-secure-session-impl (current-logger)))
  
  (define (secure-echo-server local-scurl remote-scurl in out)
    (letrec-values (((bin bout) (make-pipe))
                    ((l->b b->l) (values (lambda (lst) (list->bytes lst bin bout))
                                         (lambda (bts) (bytes->list bts bin bout)))))
      (let ((session (handle-peer-secure-response local-scurl remote-scurl in out l->b b->l)))
        (if (session? session)
            (begin
              (displayln "Successfully responded to a secure session from the remote peer.")
              (flush-output)
              (secure-echo-server-loop remote-scurl session in out l->b b->l))
            (begin
              (displayln "Failed to respond to a secure session to the remote peer.")
              (flush-output))))))
  
  (define (secure-echo-server-loop peer-scurl session in out l->b b->l)
    ; Start the echo server using the session to encrypt and decrypt.             
    (let ((edata (read in)))
      (when (bytes? edata)
        (display (string-append
                  (url->string (scurl-url peer-scurl))
                  ": Received encrypted data: "))
        (displayln edata)
        
        (let ((ddata (msg/decrypt! session edata)))
          (when (bytes? ddata)
            (let ((msg (b->l ddata)))
              (when (string? msg)
                ; Log the string to the console and send the message back.
                (displayln (string-append
                            (url->string (scurl-url peer-scurl))
                            ": Received decrypted data: "
                            msg))
                (flush-output)
                
                ; Echo the message back.
                (letrec ((d-echo-msg (l->b (string-append "ECHO: " msg)))
                         (e-echo-msg (msg/encrypt! session d-echo-msg)))
                  (displayln (string-append
                              (url->string (scurl-url peer-scurl))
                              ": Sending decrypted data: 'ECHO: "
                              msg
                              "'"))
                  (display (string-append
                            (url->string (scurl-url peer-scurl))
                            ": Sending encrypted data: "))
                  (displayln e-echo-msg)
                  (flush-output)
                  
                  (write e-echo-msg out)
                  (flush-output out)))))))
      (unless (eof-object? edata)
        (secure-echo-server-loop peer-scurl session in out l->b b->l))))
  
  (define (secure-echo-client self-scurl peer-scurl in out)
    (letrec-values (((bin bout) (make-pipe))
                    ((l->b b->l) (values (lambda (lst) (list->bytes lst bin bout))
                                         (lambda (bts) (bytes->list bts bin bout)))))
      (let ((session (handle-peer-secure-request self-scurl peer-scurl in out l->b b->l)))
        (if (session? session)
            (begin
              (displayln "Successfully requested a secure session from the remote peer.")
              (flush-output)
              (secure-echo-client-loop session peer-scurl in out l->b b->l))
            (begin
              (displayln "Failed to request a secure session from the remote peer.")
              (flush-output))))))
  
  (define (secure-echo-client-loop session peer-scurl in out l->b b->l)
    ; Read the message from standard input.
    (let ((msg (read-line)))
      (when (and
             (string? msg)
             (not (string=? msg "exit")))
        (let ((edata (msg/encrypt! session (l->b msg))))
          (display (string-append
                    (url->string (scurl-url peer-scurl))
                    ": Sending encrypted data: "))
          (displayln edata)
          (flush-output)
          
          ; Encrypt and write the bytes to output.
          (write edata out)
          (flush-output out)
          
          ; Read the echo server response from the input.
          (let ((edata (read in)))
            (when (bytes? edata)
              (display (string-append
                        (url->string (scurl-url peer-scurl))
                        ": Received encrypted data: "))
              (displayln edata)
              
              (let ((ddata (msg/decrypt! session edata)))
                (when (bytes? ddata)
                  (let ((echo-msg (b->l ddata)))
                    (when (string? echo-msg)
                      ; log the echo to the console.
                      (displayln echo-msg)
                      (flush-output)))))))))
      (unless (string=? msg "exit")
        (secure-echo-client-loop session peer-scurl in out l->b b->l))))
  
  ; Simple Secure peer.
  (define (run scurl-index)
    (let-values (((self-scurl scurls revo-certs) (read-peer-file scurl-index)))
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
        (run-peer self-scurl cert-progs revo-progs fproxy-progs rproxy-progs secure-echo-client secure-echo-server))))

