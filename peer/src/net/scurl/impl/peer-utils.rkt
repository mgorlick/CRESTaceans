#lang racket
  
  (require "../common/logging-utils.rkt"
           racket/serialize)
  
  (provide list->bytes
           bytes->list
           monitor-list-out
           monitor-bytes-in)
  
  ; Create the logger for the peer-validation-impl module.
  (define logger (make-logger 'peer-utils-logger (current-logger)))
  
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
  
  ; Returns a thread handle that monitors the list-in input-port and converts the list to a byte-string
  ; which is then sent out of the bytes-out output-port.
  (define (monitor-list-out list-in bytes-out)
    (let-values (((bytes-conversion-in bytes-conversion-out) (make-pipe)))
      (letrec ((convert
                (lambda ()
                  (if (port-closed? list-in)
                      (close-output-port bytes-out)
                      (begin
                    
                        (debug logger "Waiting to read from list-in.")
                        (let ((msg-list (read list-in)))
                          (debug logger "Received something from list-in.")
                          
                          ; Only continue if we got a list.
                          (when (or
                                 (list? msg-list)
                                 (bytes? msg-list))
                            (debug logger "Received a list from list-in, performing conversion to bytes.")
                            (letrec ((data (list->bytes msg-list bytes-conversion-in bytes-conversion-out))
                                     ; Get the length of the payload
                                     (data-length (bytes-length data))
                                     ; Concatenate the length and payload.
                                     (out-data (bytes-append
                                                (integer->integer-bytes data-length 2 #f #f)
                                                data)))
                              ; Writing a 2 byte header which specifies the length of the following payload.
                              (unless (port-closed? bytes-out)
                                (write-bytes out-data bytes-out)
                                (flush-output bytes-out)
                                
                                (debug logger (string-append "Wrote " (number->string data-length) " to bytes-out.")))))
                          (if (eof-object? msg-list)
                              (close-output-port bytes-out)
                              (when (and
                                     (not (eof-object? msg-list))
                                     (not (port-closed? list-in))
                                     (not (port-closed? bytes-out)))
                                (convert)))))))))
        (thread convert))))
  
  ; Returns a thread handle that monitors the bytes-in input-port and converts the bytes to a list
  ; which is then sent out of the list-out output-port.
  (define (monitor-bytes-in list-out bytes-in)
    (let-values (((bytes-conversion-in bytes-conversion-out) (make-pipe)))
      (letrec ((convert
                (lambda ()
                  (if (port-closed? bytes-in)
                      (close-output-port list-out)
                      (begin
                        (debug logger "Waiting to read bytes from bytes-in.")
                        ; Read the message header that is 2 bytes long.
                        (let ((msg-size-bytes (read-bytes 2 bytes-in)))
                          (debug logger "Received 2 bytes from bytes-in.")
                          
                          ; When we received 2 bytes of data and not the eof object we can continue.
                          (when (and
                                 (not (eof-object? msg-size-bytes))
                                 (bytes? msg-size-bytes)
                                 (= (bytes-length msg-size-bytes) 2))
                            ; Convert the header bytes to the size of the incoming message.
                            (let ((msg-size (integer-bytes->integer msg-size-bytes #f #f)))
                              (debug logger (string-append
                                             "Read a message header specifing an incoming paylod of "
                                             (number->string msg-size)
                                             "."))
                              
                              ; Only continue if the port is open.
                              (unless (port-closed? bytes-in)
                                (let ((msg-bytes (read-bytes msg-size bytes-in)))
                                  (debug logger (string-append "Read the payload of "
                                                               (number->string msg-size)
                                                               "."))
                                  ; Convert the bytes into a list and write it out of list-out.
                                  (when (and
                                         (not (eof-object? msg-bytes))
                                         (bytes? msg-bytes))
                                    (let ((l (bytes->list msg-bytes bytes-conversion-in bytes-conversion-out)))
                                      (unless (port-closed? list-out)
                                        (write l list-out)
                                        (flush-output list-out)
                                        
                                        (debug logger "Wrote the received bytes to list-out as a list."))))))))
                          (if (eof-object? msg-size-bytes)
                              (close-output-port list-out)
                              (when (and
                                     (not (eof-object? msg-size-bytes))
                                     (not (port-closed? bytes-in))
                                     (not (port-closed? list-out)))
                                (convert)))))))))
        (thread convert))))

