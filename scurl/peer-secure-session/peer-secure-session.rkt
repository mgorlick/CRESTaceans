#lang racket
  ; Contains the message protocol implementation for establishing a shared session
  ; between two peers.  The initiating peer should call handle-peer-secure-request
  ; and the remote peer should call handle-peer-secure-response.
  
  (require "depends.rkt"
           "crypto.rkt"
           "peer-secure-session-msgs.rkt"
           
           "../peer-validation/scurl.rkt"
           "../peer-validation/peer-validation-msgs.rkt")
  
  ; Create the logger for the peer-secure-session module.
  (define logger (make-logger 'peer-secure-session-logger peer-secure-session-parent-logger))
  
  ; The digest used when creating the shared secret key and iv.
  (define +shared-secret-digest+ digest:sha256)
  
  (define (handle-peer-secure-response local-scurl remote-scurl in out list->bytes bytes->list)
    (with-handlers ((exn? (lambda (e)
                            (error logger "Failed to respond to a secure session request." e)
                            #f)))
      
      ; Read the secure-connection-options-request-msg from the input port.
      (let ((msg (read in)))
        (cond
          ; Verify that the message is the expected type and respond with the
          ; secure-connection-options-response-msg.
          [(secure-connection-options-request-msg? msg)
           (write (secure-connection-options-response-msg) out)
           (flush-output out)
           
           ; Read the message and make sure it is the secure-connection-request-msg.
           (letrec ((data (read in))
                    (decrypted-data (if (bytes? data) (msg/decrypt/pkey local-scurl data) #f))
                    (msg (if (bytes? decrypted-data)
                             ; Successfully decrypted something!
                             (with-handlers ((exn? (lambda (e)
                                                     (error logger "Failed to convert bytes to a list." e)
                                                     #f)))
                               (bytes->list decrypted-data))
                             #f)))
             (cond
               ; Verify that the message is a secure-connection-request-msg and extract the
               ; cipher and digest types.
               [(secure-connection-request-msg? msg)
                (letrec ((cipher (secure-connection-request-msg-cipher msg))
                         (connection-response-msg (secure-connection-response-msg cipher))
                         (bytes-msg (with-handlers ((exn? (lambda (e)
                                                            (error logger "Failed to convert a list to bytes." e)
                                                            #f)))
                                      (list->bytes connection-response-msg))))
                  (if (bytes? bytes-msg)
                      (begin
                        ; Send them our generated shared keys.
                        (write (msg/encrypt/pkey remote-scurl bytes-msg) out)
                        (flush-output out)
                        
                        ; Create and return a session based upon the agreed parameters.
                        (create-session cipher
                                        +shared-secret-digest+
                                        (secure-connection-response-msg-shared-keys connection-response-msg)
                                        (secure-connection-request-msg-shared-keys msg)))
                      ; Couldn't convert the list to bytes, failed.
                      #f))]
               [else
                (debug logger "Failed to receive a valid secure-connection-request-msg.")
                ; Return the decrypted-data.
                decrypted-data]))]
          [else
           (debug logger "Failed to receive a valid secure-connection-options-request-msg.")
           ; Return what we received.
           msg]))))
  (provide/contract
   [handle-peer-secure-response (-> scurl? scurl? input-port? output-port? procedure? procedure? (or/c session? boolean?))])
  
  (define (handle-peer-secure-request local-scurl remote-scurl in out list->bytes bytes->list)
    (with-handlers ((exn? (lambda (e)
                            (debug logger "Failed to request a secure session." e)
                            #f)))
      
      ; Request the scurl information from the remote peer.
      (write (secure-connection-options-request-msg) out)
      (flush-output out)
      
      ; Read the key-response-msg which should contain the scurl information for the
      ; remote peer.
      (let ((msg (read in)))
        (cond
          [(secure-connection-options-response-msg? msg)
           
           ; Send them our half of the secure session and choose a cipher and digest
           ; to be used.
           (letrec ((cipher (find-cipher (secure-connection-options-response-msg-ciphers msg)))
                    (secure-connection-msg (secure-connection-request-msg cipher)))
             
             (letrec ((bytes-msg (with-handlers ((exn? (lambda (e)
                                                         (error logger "Failed to convert the list to bytes." e)
                                                         #f)))
                                   (list->bytes secure-connection-msg)))
                      (edata (if (bytes? bytes-msg)
                                 (msg/encrypt/pkey remote-scurl bytes-msg)
                                 #f)))
               (if (bytes? bytes-msg)
                   (begin
                     ; Successful conversion, write the bytes out.
                     (write edata out)
                     (flush-output out)
                     
                     ; Read the data, decrypt it and try to turn it into a secure-connection-response-msg.
                     (letrec ((data (read in))
                              (decrypted-data (msg/decrypt/pkey local-scurl data))
                              (msg (with-handlers ((exn? (lambda (e)
                                                           (error logger "Failed to convert bytes to a list." e)
                                                           #f)))
                                     (bytes->list decrypted-data))))
                       (cond
                         ; If the message is what we expected then we now have all the information needed
                         ; to create the shared session.
                         [(secure-connection-response-msg? msg)
                          (create-session cipher
                                          +shared-secret-digest+
                                          (secure-connection-response-msg-shared-keys msg)
                                          (secure-connection-request-msg-shared-keys secure-connection-msg))]
                         [else
                          (debug logger "Failed to receive a valid secure-connection-response-msg.")
                          ; Return the decrypted data.
                          decrypted-data])))
                   #f)))]
          [else
           (debug logger "Failed to receive a valid secure-connection-options-response-msg.")
           #f]))))
  (provide/contract
   [handle-peer-secure-request (-> scurl? scurl? input-port? output-port? procedure? procedure? (or/c session? boolean?))])
  
  ; Picks a cipher type from the list of ciphers.  This is done by applying
  ; the !cipher? function to each element in the list and the first element
  ; that returns true is then returned.  If !cipher? returns true then this
  ; system supports that cipher algorithm.  The hope is that the list of
  ; ciphers is ordered from strongest to weakest.
  (define (find-cipher ciphers)
    (cond
      ; Reached the end of the list, should this throw an error or return false?
      [(null? ciphers) (error "Failed to find an acceptable cipher.")]
      [else
       (let ((cipher (car ciphers)))
         (if (and 
              (!cipher? (car ciphers))
              (exact-positive-integer? (cipher->integer cipher)))
             ; Supported, return the first element.
             cipher
             ; Not Supported, continue on.
             (find-cipher (cdr ciphers))))]))















