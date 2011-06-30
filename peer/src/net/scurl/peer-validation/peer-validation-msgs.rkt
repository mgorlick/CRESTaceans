#lang racket
  ; This module provides the message definitions for the peer-validation protocol
  ; and creation and accessor functions for those messages.
  
  (require "depends.rkt"
           "scurl.rkt")
  
  (provide 
   PKEYS
   PKEY-IDS
   pkey->integer
   integer->pkey
   integers->pkeys
   
   DIGESTS
   DIGEST-IDS
   digest->integer
   integer->digest
   integers->digests
   
   CIPHERS
   CIPHER-IDS
   cipher->integer
   integer->cipher
   integers->ciphers
   
   signature-digest
   
   client-hello-msg
   client-hello-msg?
   client-hello-msg-max-version
   
   server-hello-msg
   server-hello-msg?
   server-hello-msg-version
   server-hello-msg-timestamp
   server-hello-msg-nonce
   
   client-auth-msg
   client-auth-msg?
   client-auth-msg-scurl
   client-auth-msg-timestamp
   client-auth-msg-nonce
   client-auth-msg-signature
   
   server-auth-msg
   server-auth-msg?
   server-auth-msg-scurl
   server-auth-msg-signature
   
   client-done-msg
   client-done-msg?
   client-done-msg-is-authenticated)
  
  ; Create a logger for this module.
  (define logger (make-logger 'peer-validation-msgs-logger peer-validation-parent-logger))
    
  ; A list of all available cipher types.
  (define CIPHERS (list cipher:aes-256 cipher:aes-128 cipher:des-ede3))
  (define CIPHER-IDS (list 1001 2001 2002))
  
  ; A list of all available digest types.
  (define DIGESTS (list digest:sha1 digest:sha256 digest:sha512))
  (define DIGEST-IDS (list 4001 4002 4003))
  
  ; A list of all available pkey types.
  (define PKEYS (list pkey:rsa))
  (define PKEY-IDS (list 5001))
  
  ; Attempts to match the requested-key value with a value in the first list
  ; and returns the correlating element from the values list.  Returns
  ; false if nothing found.
  (define (find-key-match requested-key keys values)
    (foldl (lambda (key value last)
             (if (boolean? last)
                 ; Haven't found a match yet!
                 (if (equal? requested-key key)
                     ; Matched, return the value.
                     value
                     ; Not matched, return false
                     #f)
                 ; Found a match before, return it!
                 last))
             #f
             keys
             values))
  
  ; Converts the given pkey into an integer value,
  ; false is returned when the given pkey is not
  ; supported.
  (define (pkey->integer key)
    (find-key-match key PKEYS PKEY-IDS))
  
  ; Converts the given integer value into a pkey object,
  ; false is returned when the given value is unknown.
  (define (integer->pkey value)
    (find-key-match value PKEY-IDS PKEYS))
  
  ; Converts a list of integers into a list of pkey's if possible, or booleans.
  (define (integers->pkeys values)
    (map integer->pkey values))
  
  ; Converts the given digest into an integer value,
  ; false is returned when the given digest is not supported.
  (define (digest->integer digest)
    (find-key-match digest DIGESTS DIGEST-IDS))
  
  ; Converts the given integer into a digest object,
  ; false is returned when the given value is unknown.
  (define (integer->digest value)
    (find-key-match value DIGEST-IDS DIGESTS))
  
  ; Converts a list of integers into a list of digest's if possible, or booleans.
  (define (integers->digests values)
    (map integer->digest values))
  
  ; Converts the given cipher into an integer value,
  ; false is returned when the given cipher is not supported.
  (define (cipher->integer cipher)
    (find-key-match cipher CIPHERS CIPHER-IDS))
  
  ; Converts the given integer into a cipher object,
  ; false is returned when the given cipher is unknown.
  (define (integer->cipher value)
    (find-key-match value CIPHER-IDS CIPHERS))
  
  ; Converts a list of integers into a list of cipher's if possible, or booleans.
  (define (integers->ciphers values)
    (map integer->cipher values))
  
  ; The digest algorithm that is used to sign and verify the Host ID.
  (define +signature-digest+ digest:sha256)
  (define (signature-digest)
    +signature-digest+)
  
  ; Message id and message for the client hello message.
  ; This message is used to intiate the authentication protocol.
  (define +client-hello-msg-id+ 5001)
  
  ; Returns a client-hello-msg that contains the given version
  ; which defines the largest supported version number for the
  ; protocol.
  ; Returns a list with id (int) and version (real)
  (define (client-hello-msg max-version)
    (list +client-hello-msg-id+
          max-version))
  
  ; Returns true when the given item is a list and has the correct
  ; number of members and the correct types.
  (define (client-hello-msg? msg)
    (if (and
         (list? msg)
         (= (length msg) 2))
        (let ((msg-id (first msg))
              (max-version (second msg)))
          (and (equal? msg-id +client-hello-msg-id+)
               (number? max-version)))
        ; Not enough data members
        #f))
  
  ; Returns the version maximum version supported by the
  ; client.
  (define (client-hello-msg-max-version msg)
    (if (client-hello-msg? msg)
        ; Extract and return the version.
        (second msg)
        ; Not a valid message.
        #f))
  
  ; Message id and message for the server hello message.
  ; This message is sent in response to a client hello
  ; message.
  (define +server-hello-msg-id+ 6001)
  
  ; Returns a server-hello-msg that contains the chosen
  ; version and a timestamp/nonce that the client must use
  ; to create a signature.
  ; Returns a list with id (int), timestamp (uint),
  ; nonce (bytes)
  (define (server-hello-msg version timestamp nonce)
    (list +server-hello-msg-id+
          version
          timestamp
          nonce))
  
  ; Returns true when the given item is a list with
  ; the correct length and types.
  (define (server-hello-msg? msg)
    (if (and
         (list? msg)
         (= (length msg) 4))
        ; Its a list with the correct length, extract
        ; the values and check the types.
        (let ((msg-id (first msg))
              (version (second msg))
              (timestamp (third msg))
              (nonce (fourth msg)))
          (and (equal? msg-id +server-hello-msg-id+)
               (number? version)
               (exact-positive-integer? timestamp)
               (bytes? nonce)))
        ; Failed to be a list or have the correct length.
        #f))
  
  ; Returns the version specified in this message.
  (define (server-hello-msg-version msg)
    (if (server-hello-msg? msg)
        ; Valid, return the version item.
        (second msg)
        ; Failed, return false.
        #f))
  
  ; Returns the timetsamp specified in this message.
  (define (server-hello-msg-timestamp msg)
    (if (server-hello-msg? msg)
        ; Valid, return the timestamp item.
        (third msg)
        ; Failed, return false.
        #f))
  
  ; Returns the nonce specified in this message.
  (define (server-hello-msg-nonce msg)
    (if (server-hello-msg? msg)
        ; Valid, return the nonce.
        (fourth msg)
        ; Failed, return false
        #f))
  
  ; Message id and message for the key request message.
  ; This message is used to initiate a connection request.
  (define +client-auth-msg-id+ 5002)
  
  ; Returns a client-auth-msg that contains the scurl information,
  ; a signature, a timestamp and nonce.
  ; Returns a list with id (int), scurl (string), digest (int),
  ; pkey (int), key (bytes), timestamp (uint), nonce (bytes) and signature (bytes)
  (define (client-auth-msg scurl timestamp nonce signature)
    (list +client-auth-msg-id+
          (url->string (scurl-url scurl))
          (digest->integer (scurl-digest-type scurl))
          (pkey->integer (scurl-key-type scurl))
          (public-key->bytes (scurl-key scurl))
          timestamp
          nonce
          signature))
  
  ; Returns true when the given item is a list with the proper message id
  ; and proper element types.
  (define (client-auth-msg? msg)
    (if (and
         (list? msg)
         (= (length msg) 8))
        (let ((msg-id (first msg))
              (url (second msg))
              (digest-type-id (third msg))
              (pkey-type-id (fourth msg))
              (key (fifth msg))
              (timestamp (sixth msg))
              (nonce (seventh msg))
              (sig (eighth msg)))
          (and
           (equal? msg-id +client-auth-msg-id+)
           (string? url)
           (exact-positive-integer? digest-type-id)
           (exact-positive-integer? pkey-type-id)
           (bytes? key)
           (exact-positive-integer? timestamp)
           (bytes? nonce)
           (bytes? sig)))
        ; Not enough elements.
        #f))
  
  ; Extracts the scurl components from the given list which should have
  ; been created from the client-auth-msg function and creates a scurl
  ; from them.
  ; Returns false if the scurl cannot be created.
  (define (client-auth-msg-scurl msg)
    ; Verify that we have the correct message first.
    (if (client-auth-msg? msg)
        ; Extract the items and perform conversions to types.
        (let ((url (second msg))
              (digest-type (integer->digest (third msg)))
              (key-type (integer->pkey (fourth msg)))
              (key-bytes (fifth msg)))
          ; Make sure all of the conversions went smoothly.
          (if (and
               (!digest? digest-type)
               (!pkey? key-type))
              ; Attempt to create a pkey object.
              (let ((key (with-handlers
                             ((exn? (lambda (e)
                                      (debug logger 
                                             "Failed to create a pkey out of the client-auth-msg."
                                             e)
                                      #f)))
                           (bytes->public-key key-type key-bytes))))
                (if (pkey? key)
                    ; Let's try to create a scurl.
                    (string->scurl url digest-type key-type key)
                    (begin
                      (debug logger "Failed to create a pkey out of the client-auth-msg.")
                      #f)))
              (begin
                (debug logger "The digest-type-id or pkey-type-id failed to convert to a valid type.")
                #f)))
        (begin
          (debug logger "The given item was not a valid client-auth-msg.")
          #f)))
  
  ; Returns the timestamp from the given list which should have been
  ; created with the client-auth-msg function.
  ; Returns false if the list is not long enough or the element is not valid.
  (define (client-auth-msg-timestamp msg)
    ; Make sure we have enough elements in the list.
    (if (client-auth-msg? msg)
        ; Valid, return the timestamp field.
        (sixth msg)
        ; Failed, return false.
        #f
        ))
  
  ; Returns the nonce from the given list which should have been
  ; created with the client-auth-msg function.
  ; Returns false if the list is not long enough or the element is not valid.
  (define (client-auth-msg-nonce msg)
    ; Make sure we have enough elements in the list.
    (if (client-auth-msg? msg)
        ; Valid, return the nonce element.
        (seventh msg)
        ; Failed, return false
        #f))
  
  ; Returns the signature from the given list which should have been
  ; created with the client-auth-msg function.
  ; Returns false if the list is not long enough or the element is not valid.
  (define (client-auth-msg-signature msg)
    ; Make sure we have enough elements in the list.
    (if (client-auth-msg? msg)
        ; Valid, return the signature element.
        (eighth msg)
        ; Failed, return false.
        #f))
  
  ; Message id and message for the server authenticate message.
  ; This message is used in response to a client-auth-msg and this is
  ; what the requesting peer will be expecting to receive.  The scurl
  ; will contain the host-id which can be used to verify the signature.
  (define +server-auth-msg-id+ 6002)
  
  ; Returns a server-auth-msg that contains the scurl information
  ; and a signature.
  ; Returns a list with id (int), scurl (string), digest (int),
  ; pkey (int), key (bytes) and signature (bytes)
  (define (server-auth-msg scurl signature)
    (list +server-auth-msg-id+
          (url->string (scurl-url scurl))
          (digest->integer (scurl-digest-type scurl))
          (pkey->integer (scurl-key-type scurl))
          (public-key->bytes (scurl-key scurl))
          signature))
  
  ; Returns true when the given item is a list with the proper message id
  ; and proper element types.
  (define (server-auth-msg? msg)
    (if (and
         (list? msg)
         (= (length msg) 6))
        (let ((msg-id (first msg))
              (url (second msg))
              (digest-type-id (third msg))
              (pkey-type-id (fourth msg))
              (key (fifth msg))
              (sig (sixth msg)))
          (and
           (equal? msg-id +server-auth-msg-id+)
           (string? url)
           (exact-positive-integer? digest-type-id)
           (exact-positive-integer? pkey-type-id)
           (bytes? key)
           (bytes? sig)))
        ; Not enough elements.
        #f))
  
  ; Extracts the scurl components from the given list which should have
  ; been created from the server-auth-msg function and creates a scurl
  ; from them.
  ; Returns false if the scurl cannot be created.
  (define (server-auth-msg-scurl msg)
    ; Verify that we have the correct message first.
    (if (server-auth-msg? msg)
        ; Extract the items and perform conversions to types.
        (let ((url (second msg))
              (digest-type (integer->digest (third msg)))
              (key-type (integer->pkey (fourth msg)))
              (key-bytes (fifth msg)))
          ; Make sure all of the conversions went smoothly.
          (if (and
               (!digest? digest-type)
               (!pkey? key-type))
              ; Attempt to create a pkey object.
              (let ((key (with-handlers
                             ((exn? (lambda (e)
                                      (debug logger 
                                             "Failed to create a pkey out of the server-auth-msg."
                                             e)
                                      #f)))
                           (bytes->public-key key-type key-bytes))))
                (if (pkey? key)
                    ; Let's try to create a scurl.
                    (string->scurl url digest-type key-type key)
                    (begin
                      (debug logger "Failed to create a pkey out of the server-auth-msg.")
                      #f)))
              (begin
                (debug logger "The digest-type-id or pkey-type-id failed to convert to a valid type.")
                #f)))
        (begin
          (debug logger "The given item was not a valid server-auth-msg.")
          #f)))
  
  ; Returns the signature from the given list which should have been
  ; created with the server-auth-msg function.
  ; Returns false if the list is not long enough or the element is not valid.
  (define (server-auth-msg-signature msg)
    ; Make sure we have enough elements in the list.
    (if (server-auth-msg? msg)
        ; Valid, return the signature element.
        (sixth msg)
        ; Failed, return false.
        #f))
  
  ; The message id and message for the client-done-msg.
  ; Most likely the fact that this message was received as opposed
  ; to the socket being closed means that the connection
  ; is allowed.
  (define +client-done-msg-id+ 5003)
  (define (client-done-msg is-authenticated)
    (list +client-done-msg-id+ is-authenticated))
  
  ; Returns true when the given msg is a list with the correct message id.
  (define (client-done-msg? msg)
    (if (and
         (list? msg)
         (= (length msg) 2))
        (let ((msg-id (first msg))
              (is-authenticated (second msg)))
          (and
           (equal? msg-id +client-done-msg-id+)
           (boolean? is-authenticated)))
        ; Not enough elements.
        #f))
  
  ; Returns the is-authenticated element of the given client-done-msg
  ; or a boolean value of false if the given list is not a well-formed
  ; connection-validation-msg.
  (define (client-done-msg-is-authenticated msg)
    (if (client-done-msg? msg)
        ; Valid, return the is-authenticated element.
        (second msg)
        ; Failed, return false.
        #f))

