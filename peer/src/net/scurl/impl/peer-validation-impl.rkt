#lang racket/base

(require racket/contract racket/list)
  
  (require "../peer-validation/depends.rkt"
           "../peer-validation/scurl.rkt"
           "../peer-validation/host-id.rkt"
           "../peer-validation/peer-validation.rkt"
           "../peer-revocation/revo-cert.rkt"
           "../services/revocation.rkt"
           "../services/forward-proxy.rkt"
           "../services/program.rkt"
           "../services/certification.rkt"
           "../services/reverse-proxy.rkt"
           "../common/logging-utils.rkt"
           "generate-test-scurls.rkt"
           "peer-utils.rkt"
           racket/tcp)
  
  (provide (except-out (all-defined-out) logger))
  
  ; Create the logger for the peer-validation-impl module.
  (define logger (make-logger 'peer-validation-impl (current-logger)))
  
  (define (run-peer self-scurl cert-progs revo-progs fproxy-progs rproxy-progs [request-service echo-client] [response-service echo-server])
    ; Read the scurl's from the local list.
    (letrec (; This is the handle-revocation function. We will delegate revocation to
             ; the revocation module.
             (revo-func (lambda (scurl) (run-revo-programs revo-progs scurl)))
             ; Start up the tcp listener on our port and then listen on the command prompt.
             (port (if (boolean? (url-port (scurl-url self-scurl)))
                       8080
                       (url-port (scurl-url self-scurl))))
             (listener (tcp-listen port)))
      ; Launch a thread which handles any incoming requests.
      (let ((request-thread (thread (lambda () (handle-incoming-requests self-scurl revo-func rproxy-progs listener response-service)))))
        ; Call the local request handler on the current thread.
        (handle-local-request-input self-scurl cert-progs revo-progs fproxy-progs request-service)
        ; If we exit from local control then we are done, kill the request listener thread and shutdown
        ; the tcp port.
        (kill-thread request-thread)
        (tcp-close listener))))
  
  ; Handles incoming requests from the tcp port.  For each connection spawn a thread to handle the connection
  ; request and service then wait for another connection.
  (define (handle-incoming-requests self-scurl revo-func rproxy-progs listener service)
    ; Wait for a tcp connection and create the pipes needed for conversion when
    ; we get a connection.
    (letrec-values (((bytes-in bytes-out) (tcp-accept listener)))
      ; Start a thread to handle the request
      (thread (lambda ()
                (handle-incoming-request self-scurl revo-func rproxy-progs bytes-in bytes-out service)

                ; Clean up resources
                (close-output-port bytes-out)
                (close-input-port bytes-in))))
    
    ; Do it again.
    (handle-incoming-requests self-scurl revo-func rproxy-progs listener service))
  
  ; Handle a single incoming connection from a remote peer.  Create the conversion pipes
  ; and kick them off background monitoring threads.
  (define (handle-incoming-request self-scurl revo-func rproxy-progs bytes-in bytes-out service)
    (debug logger "handle-incoming-request: received socket connection.")
    
    ; Create the pipes to handle bytes/lists conversions.
    (let-values (((mon-list-in list-out) (make-pipe))
                 ((list-in mon-list-out) (make-pipe)))
      ; Start the monitoring threads that convert from list to bytes and bytes to lists.
      (let ((mlo (monitor-list-out mon-list-in bytes-out))
            (mbi (monitor-bytes-in mon-list-out bytes-in)))
        
        (debug logger "handle-incoming-request: Attempting to handle-peer-validation-response.")
        (let ((scurl (handle-server-authentication
                      self-scurl
                      revo-func
                      list-in
                      list-out)))
          ; Finished peer-validation, let's see what we got.
          (if (scurl? scurl)
              (begin
                (displayln (string-append 
                            "Successfully responded to an incoming peer-validation request from peer ("
                            (url->string (scurl-url scurl))
                            ")"))
                (flush-output)
                
                ; Before running the service, run the reverse proxy service to see if any handlers take
                ; care of the message for us.
                (let ((output (run-rproxy-programs rproxy-progs scurl (list list-in list-out))))
                  ; If the output is a boolean value of false then the message has not been handled and we should start
                  ; the service.
                  (if (equal? output #f)
                      (begin
                        (service self-scurl scurl list-in list-out)
                        (displayln (string-append
                                    "The connection to peer "
                                    (url->string (scurl-url scurl))
                                    " has been closed."))
                        (flush-output))
                      (begin
                        (displayln (string-append
                                    "The connection from peer "
                                    (url->string (scurl-url scurl))
                                    " has been handled by a reverse proxy handler."))
                        (flush-output)))))
              (begin
                (displayln (string-append
                            "Refused an incoming peer-validation request from a peer."))
                (flush-output)))
          
          ; All done, let's cleanup our resources.
          (kill-thread mlo)
          (kill-thread mbi)
          
          (close-output-port list-out)
          (close-output-port mon-list-out)
          
          (close-input-port list-in)
          (close-input-port mon-list-in)))))
  
  ; Reads input from the command line and if the input is not "exit" then the handle-local-request
  ; function is called and input will be read again.  If "exit" is entered then the local part
  ; of the client will exit.
  (define (handle-local-request-input self-scurl cert-progs revo-progs fproxy-progs service)
    (let ((cmd (read-line)))
      (when (and (string? cmd)
                 (not (string=? (string-downcase cmd) "exit")))
        (handle-local-request cmd self-scurl cert-progs revo-progs fproxy-progs service)
        (handle-local-request-input self-scurl cert-progs revo-progs fproxy-progs service))))
  
  ; Reads local console input looking for either an 'e' to exit or scurl text.  When a scurl text
  ; is given then the forward proxy service, certification service and revocation service will
  ; be run prior to attempting to connect to the remote peer.
  (define (handle-local-request cmd self-scurl cert-progs revo-progs fproxy-progs service)
    ; Turn the string into an url.
    (let ((url (string->url cmd)))
      ; First run the forward proxy service to see if we need to redirect the connection
      ; to a different url.
      (let* ((output (run-fproxy-programs fproxy-progs url))
             ; If we have a boolean value then we did not find a forward proxy.
             (url (if (boolean? output) url output)))
        
        (when (equal? url output)
          (displayln (string-append "A forward proxy handler converted "
                                    cmd
                                    " to "
                                    (if (url? url) (url->string url) (url->string (scurl-url url)))))
          (flush-output))
        
        ; Next we need to run the certification service if this is an url to see if we can turn
        ; it into a scurl.
        (let* ((output (if (scurl? url) url (run-cert-programs cert-progs url)))
               (url (if (boolean? output) url output)))
          
          (when (equal? url output)
            (displayln (string-append "A certification handler converted the url to "
                                      (if (url? url) (url->string url) (url->string (scurl-url url)))))
            (flush-output))
          
          ; Next we need to run the revocation service.
          (let* ((output (run-revo-programs revo-progs url))
                 (block (or (revo-cert? output)
                            (and (boolean? output) output))))
            ; If we are not blocked, let's attempt to open up a connection.
            (if (not block)
                (handle-outgoing-request self-scurl url service)
                (begin
                  (if (revo-cert? output)
                      (displayln "A revocation handler returned a revocation certificate to block access to this scurl.")
                      (displayln "A revocation handler performed host-id blocking to block access to this scurl."))
                  (flush-output))))))))
  
  ; Attempts to connect to the specified peer-scurl.
  (define (handle-outgoing-request self-scurl remote-url service)
    (debug logger "handle-outgoing-request: Attempting to connect to a remote peer socket.")
    ; Extract the host and port from the peer-scurl.
    (let* ((url (if (scurl? remote-url) (scurl-url remote-url) remote-url))
           (host (url-host url))
           (port (if (boolean? (url-port url))
                     8080
                     (url-port url))))
      ; Open a tcp connection to the peer-scurl.
      (letrec-values (((bytes-in bytes-out) (with-handlers ((exn? (lambda (e)
                                                                    (debug logger "Failed to connect to the given host and port." e)
                                                                    (values #f #f))))
                                              (tcp-connect host port)))
                      ((mon-list-in list-out) (make-pipe))
                      ((list-in mon-list-out) (make-pipe)))
        (unless (and
                 (boolean? bytes-in)
                 (boolean? bytes-out))
      
          (debug logger "handle-outgoing-request: Successfully connected through tcp to the remote peer socket.")
          
          ; Start the threads that convert between list and bytes.
          (let ((mlo (monitor-list-out mon-list-in bytes-out))
                (mbi (monitor-bytes-in mon-list-out bytes-in)))
            
            (let ((scurl (handle-client-authentication self-scurl remote-url list-in list-out)))
              (if (scurl? scurl)
                  ; If we didn't know their scurl to begin with then we probably want to store this scurl
                  ; somewhere.
                  (begin
                    (displayln (string-append 
                                "Successfully created a connection by requesting a peer-validation with peer ("
                                (url->string (scurl-url scurl))
                                ")"))
                    (flush-output)
                    
                    ; Start the echo client.
                    (service self-scurl remote-url list-in list-out)
                    (displayln (string-append
                                "The connection to peer "
                                (url->string (scurl-url scurl))
                                " has been closed."))
                    (flush-output))
                  (begin
                    (displayln (string-append
                                "The peer-validation request was refused by the peer."))
                    (flush-output)))
              
              ; Clean up our resources.
              (kill-thread mlo)
              (kill-thread mbi)
              
              (close-output-port list-out)
              (close-output-port mon-list-out)
              (close-output-port bytes-out)
              
              (close-input-port list-in)
              (close-input-port mon-list-in)
              (close-input-port bytes-in)))))))
  
  (define (echo-server self-scurl peer-scurl in out)
    (let ((msg-list (read in)))
      (when (and 
             (list? msg-list)
             (> (length msg-list) 0))
        (let ((msg (car msg-list)))
          (when (string? msg)
            ; Log the string to the console and send the message back.
            (displayln (string-append
                        (url->string (scurl-url peer-scurl))
                        ": "
                        msg))
            (flush-output)
            
            (write (list (string-append "ECHO: " msg)) out)
            (flush-output out))))
      (unless (eof-object? msg-list)
        (echo-server self-scurl peer-scurl in out))))
  
  (define (echo-client self-scurl peer-scurl in out)
    ; Read the message from standard input.
    (let ((msg (read-line)))
      (when (and
             (string? msg)
             (not (string=? msg "exit")))
        (write (list msg) out)
        (flush-output out)
        
        ; Read from the echo from the input.
        (let ((msg-list (read in)))
          (when (and
                 (list? msg-list)
                 (> (length msg-list) 0))
            (let ((echo-msg (car msg-list)))
              ; log the echo to the console.
              (displayln echo-msg)
              (flush-output)))))
      (unless (string=? msg "exit")
        (echo-client self-scurl peer-scurl in out))))

