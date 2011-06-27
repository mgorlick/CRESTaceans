#lang racket
  
  (require "depends.rkt"
           "revo-cert.rkt"
           "revocation-msgs.rkt"
           
           "../peer-validation/scurl.rkt"
           "../peer-validation/peer-validation.rkt"
           "../peer-validation/peer-validation-msgs.rkt")
  
  (provide handle-server-revocation-response)
  
  ; Create the logger for the peer-revocation module.
  (define logger (make-logger 'peer-revocation-logger revocation-parent-logger))
  
  ; The handle-server-revocation-response function takes three parameters.
  ; local-scurl - the scurl describing the local peer.
  ; in - an input-port used to read incoming messages from the remote peer.
  ; out - an output-port used to write outgoing messages to the remote peer.
  ;
  ; This function will send a key-revocation-response-msg when a client-hello-msg
  ; is received.  This should inform them that the scurl they were trying to
  ; connect to has been compromised.
  ;
  ; If a key-revocation-response-msg is sent then true will be returned, false otherwise.
  ;
  ; private-scurl? input-port? output-port? -> boolean?
  (define (handle-server-revocation-response local-scurl in out)
    ; Catch all handler, something we did not expect happened, return false.
    (with-handlers ((exn? (lambda (e)
                            (error logger 
                                     (string-append
                                      "Exception occured while attempting to "
                                      "send a key-revocation-response-msg in "
                                      "response to a remote peer's incoming request.")
                                     e)
                            #f)))
      
      ; Read the client-hello-msg from the input port.
      (let ((msg (read in)))
        (cond
          ; Verify that the message is the a client-hello-msg.
          [(client-hello-msg? msg)
           ; Send them the revocation message.
           (write (key-revocation-response-msg (scurl->revo-cert local-scurl)) out)
           (flush-output)
           
           ; Return true to indicate success.
           #t]
          [else
           ; Don't know what we got.
           (debug logger "Failed to receive a client-hello-msg.")
           #f]))))

