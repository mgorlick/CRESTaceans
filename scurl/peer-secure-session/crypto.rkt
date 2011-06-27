#lang racket
  ; Two methods are provided for encryption, one using a scurl structure
  ; that performs encryption using the enclosed pkey and one using a
  ; session structure that performs encryption using the enclosed ciphers.
  ; Two methods are provided for decryption which mirror the methods for
  ; encryption.
  
  (require "depends.rkt"
           "../peer-validation/scurl.rkt")
  
  ; Create the logger for the crypto module.
  (define logger (make-logger 'crypto-logger peer-secure-session-parent-logger))
  
  ; A structure that captures the shared key information needed to generate
  ; a shared session used during symmetric encryption between two peers.
  (struct shared-keys (kcs ivcs ksc ivsc))
  (provide/contract
   [struct shared-keys ((kcs bytes?)
                        (ivcs bytes?)
                        (ksc bytes?)
                        (ivsc bytes?))])
  
  ; This function generates a shared-keys structure with randomly generated
  ; bytes.  The kcs and ksc length will be half of the key-size as
  ; will the ivcs and ivsc length will be half of the iv-size as specified
  ; by the given cipher-type.
  (define (generate-shared-keys cipher-type)
    ; Grab the size of the key and iv and divide by two.
    (let ((hkey ( / (cipher-key-length cipher-type) 2))
          (hiv (/ (cipher-iv-length cipher-type) 2)))
      ; Generate random data for the shared-key structure.
      (shared-keys (random-bytes hkey)
                   (random-bytes hiv)
                   (random-bytes hkey)
                   (random-bytes hiv))))
  (provide/contract
   [generate-shared-keys (-> !cipher? shared-keys?)])
  
  ; The session structure defines a crypto-stream that is
  ; used for encryption and decryption of data.  The digest
  ; and key are stored for use when computing an hmac value
  ; for a set of bytes.
  (struct session (cipher-type digest-type key octx ictx timestamp))
  (provide/contract
   [struct session ((cipher-type !cipher?)     ; The type of cipher used to create the crypto-streams.
                    (digest-type !digest?)     ; The type of digest used when computing an hmac on data.
                    (key bytes?)               ; A byte stream used as the key when computing the hmac.
                    (octx cipher?)             ; The crypto stream used for encryption.
                    (ictx cipher?)             ; The crypto stream used for decryption.
                    (timestamp exact-integer?) ; The time at which this session was created.
                    )])
  
  ; This function takes the two shared-keys and performs three hashes
  ; to yield a shared session.
  ;
  ; Below is the computation that is performed:
  ;   key = hash ( append ( hash (kcs,kcs) , hash (ksc, ksc) ) )
  ;   iv  = hash ( append ( hash (ivcs, ivcs), hash (ivsc, ivsc) ) )
  ; 
  (define (create-session cipher-type digest-type keys1 keys2)
    ; Append the keys together and performs some digests to create a shared
    ; key and initialization vector.
    (let ((key (digest digest-type
                       (bytes-append 
                        [digest digest-type (bytes-append (shared-keys-kcs keys1)
                                                          (shared-keys-kcs keys2))]
                        [digest digest-type (bytes-append (shared-keys-ksc keys1)
                                                          (shared-keys-ksc keys2))])))
          (iv (digest digest-type
                      (bytes-append
                       (digest digest-type (bytes-append (shared-keys-ivcs keys1)
                                                         (shared-keys-ivcs keys2)))
                       (digest digest-type (bytes-append (shared-keys-ivsc keys1)
                                                         (shared-keys-ivsc keys2))))))
          (key-length (cipher-key-length cipher-type))
          (iv-length (cipher-iv-length cipher-type)))
      (session cipher-type
               digest-type
               key
               ; Do not let the cipher pad the data for us, we need to do
               ; the padding on our own.  This is a bug in the crypto lib.
               ; Once the padding is fixed, allow them to perform padding.
               (cipher-encrypt cipher-type (subbytes key 0 key-length) (subbytes iv 0 iv-length) #:padding #f)
               (cipher-decrypt cipher-type (subbytes key 0 key-length) (subbytes iv 0 iv-length) #:padding #f)
               ; Set the timestamp to current time, this will let them reinitialize
               ; the session when the timestamp expires.
               (current-seconds))))
  (provide/contract
   [create-session (-> !cipher? !digest? shared-keys? shared-keys? session?)])
  
  ; Looked this up in the openssl/crypto library.
  ; The RSA encryption needs 11 bytes for some type
  ; of information.  This means our max block size
  ; for RSA encryption is (pkey-size key) - 11.
  (define +rsa-header-size+ 11)
  
  ; There seems to be another bug in the crypto library, this function
  ; is covering it up for us.
  (define (msg/encrypt/pkey-size scurl)
    (let ((pkey (scurl-key scurl))
          (key-type (scurl-key-type scurl)))
      (cond
        [(eq? pkey:rsa key-type)
         ; The rsa key needs the block to be less then the reported
         ; block size, the crypto library doesn't account for the rsa header.
         ; Looked this up in the openss/crypto library.
         (- (pkey-size pkey) +rsa-header-size+)]
        [else
         ; For others the pkey-size might be alright, not using dsa though.
         (pkey-size pkey)])))
  
  ; Returns the block size for the pkey wrapped in the given scurl.
  (define (msg/decrypt/pkey-size scurl)
    (let ((pkey (scurl-key scurl)))
      ; For decryption, report the pkey-size as returned by the crypto lib.
      ; We do not need to subtract the rsa header size here.
      (pkey-size pkey)))
  
  ; Define the number of bytes used to store the message length when
  ; encrypting or decrypting data.
  (define +message-length-in-bytes+ 4)
  (define (message-length-in-bytes)
    +message-length-in-bytes+)
  (provide/contract
   [+message-length-in-bytes+ exact-positive-integer?])
  
  ; The msg/encrypt/pkey function creates a payload by appending meta-data
  ; to the given msg data.  A mac is created by appending the data length
  ; to the data.  The final payload is a byte-string with the mac, length
  ; and data appended in that order.
  (define (msg/encrypt/pkey scurl msg)
    (letrec ((digest-type (scurl-digest-type scurl))
             (key-bytes (public-key->bytes (scurl-key scurl)))
             
             ; Find the data length
             (length (msg-length msg))
             ; Create a mac using the length and data as the hmac base.
             (mac (hmac digest-type key-bytes (bytes-append length msg)))
             ; Create the payload from the mac, length and data.
             (data (bytes-append mac length msg)))
      ; Finally, encrypt the payload.
      (msg/encrypt/pkey/batch scurl data)))
  (provide/contract
   [msg/encrypt/pkey (-> scurl? bytes? (or/c boolean? bytes?))])
  
  ; Encrypts the given data at the correct batch size for the pkey.
  (define (msg/encrypt/pkey/batch scurl data)
    (letrec ((key (scurl-key scurl))
             ; Grab the batch size for the given key and key-type.
             (batch-size (msg/encrypt/pkey-size scurl))
             ; Define a recursive function that encrypts the data based upon the batch size
             ; and appends the encrypted bytes together.
             (process (lambda (data data-index)
                        ; Find the ending index for this batch
                        (let ((data-end (+ data-index batch-size)))
                          ; Check if this is the last batch or not.
                          (if (<= (- (bytes-length data) data-end) 0)
                              ; Encrypt and return the encrypted data.
                              (encrypt/pkey key data data-index)
                              ; Encrypt and append the next batch.
                              (bytes-append (encrypt/pkey key data data-index data-end)
                                            (process data (+ data-index batch-size))))))))
      ; Attempt to encrypt and catch all errors.
      (with-handlers ((exn? (lambda (e)
                              (error logger 
                                     "msg/encrypt/pkey/batch: Exception occured while attempting to encrypt the given data."
                                     e)
                              #f)))
        (process data 0))))
  
  ; The msg/decrypt/pkey function decrypts data that was encrypted by
  ; the msg/encrypt/pkey function.  The data is decrypted with the
  ; given scurl and then broken up into the mac, msg length and msg.
  ; A new mac is computed similar to the msg/encrypt/pkey function
  ; and verified against the hash pulled out of the decrypted data.
  ; The length of the msg is also verified with the length pulled
  ; out of the decrypted data.  If everything is valid then the decrypted
  ; msg is returned, but if something is invalid than a boolean value
  ; of false is returned.
  (define (msg/decrypt/pkey scurl data)
    (let ((msg (msg/decrypt/pkey/batch scurl data)))
      (if (boolean? msg)
          (begin
            (error logger "Failed to decrypt the given data.")
            #f)
          (letrec ((digest (scurl-digest-type scurl))
                   (pk (scurl-key scurl))
                   
                   ; Extract the mac.
                   (dsize (digest-size digest))
                   (mac (subbytes msg 0 dsize))
                   ; Extract the message length.
                   (length-bytes (subbytes msg dsize (+ dsize (message-length-in-bytes))))
                   (length (bytes->msg-length length-bytes))
                   ; Extract the message.
                   (msgout (subbytes msg (+ dsize (message-length-in-bytes)))))
            (cond
              ; Compute a new mac and make sure it is what was sent and compare the
              ; message lengths.
              [(and 
                ; Verify that the mac is the valid.
                (bytes=? mac (hmac digest (public-key->bytes pk) (bytes-append length-bytes msgout)))
                ; Verify that the length is valid.
                (= length (bytes-length msgout)))
               msgout]
              [else
               (debug logger "msg/decrypt/pkey: The mac or message length was incorrect when decrypting.")
               #f])))))
  (provide/contract
   [msg/decrypt/pkey (-> scurl? bytes? (or/c bytes? boolean?))])
  
  ; Decrypts the given data at the correct batch size for the pkey.
  (define (msg/decrypt/pkey/batch scurl data)
    (letrec ((key (scurl-key scurl))
             (batch-size (msg/decrypt/pkey-size scurl))
             ; Define a recursive function that decrypts the data based upon the batch size
             ; and appends the decrypted bytes together.
             (process (lambda (data data-index)
                        ; Find the end index for the current batch.
                        (let ((data-end (+ data-index batch-size)))
                          ; Check to see if this is the last batch.
                          (if (<= (- (bytes-length data) data-end) 0)
                              ; Last batch, return the decrypted data.
                              (decrypt/pkey key data (min data-index (bytes-length data)))
                              ; Decrypt the batch and append it to the next decrypted batch.
                              (bytes-append (decrypt/pkey key data data-index data-end)
                                            (process data (+ data-index batch-size))))))))
      ; Attempt to encrypt and catch all errors.
      (with-handlers ((exn? (lambda (e)
                              (error logger 
                                     "msg/decrypt/pkey/batch: Exception occured while attempting to decrypt the given data."
                                     e)
                              #f)))
        (process data 0))))
  
  ; The msg/encrypt! function encrypts the msg along with the
  ; length and a hash of the msg.  The mac, length and msg are appended to
  ; form the payload.  The mac is based off of the length and msg appended together.
  (define (msg/encrypt! session msg)
    (letrec ((octx (session-octx session))
             (cipher-type (session-cipher-type session))
             (digest-type (session-digest-type session))
             (key (session-key session))
             
             ; Grab the length of the data.
             (length (msg-length msg))
             ; Create the base data used to create the mac.
             (mac-data (bytes-append length msg))
             ; Create the mac.
             (mac (hmac digest-type key mac-data))
             ; Create the payload.
             (data (bytes-append mac mac-data)))
      ; Add padding prior to encryption to ensure that the data is of a valid
      ; length.
      (with-handlers ((exn? (lambda (e)
                              (error logger 
                                     "msg/encrypt!: Exception occured while attempting to encrypt the given data."
                                     e)
                              #f)))
        (cipher-update! octx (add-padding data (cipher-block-size cipher-type))))))
  (provide/contract
   [msg/encrypt! (-> session? bytes? (or/c boolean? bytes?))])
  
  ; The msg/decrypt! function decrypts data that was encrypted by
  ; the msg/encrypt! function.  The data is decrypted with the
  ; given key and then broken up into the hash, msg length and msg.
  ; A new hash is computed similar to the msg/encrypt function
  ; and verified against the hash pulled out of the decrypted data.
  ; The length of the msg is also verified with the length pulled
  ; out of the decrypted data.  If everything is valid then the decrypted
  ; msg is returned, but if something is invalid than a boolean value
  ; of false is returned.
  (define (msg/decrypt! session data)
    ; First check to see that the given data is a valid length when compared with our
    ; encryption algorithm's block size.
    (if (not (exact-integer? (/ (bytes-length data)
                                (cipher-block-size (session-cipher-type session)))))
        (begin
          (debug logger (string-append
                         "msg/decrypt!: The size of the given data is not valid based upon "
                         "the cipher block size.  Data might have been added or removed."))
          #f)
        ; Everything seems alright, let's attempt to decrypt.
        (let ((decrypted-msg
               ; Attempt to decrypt and catch all errors.
               (with-handlers ((exn? (lambda (e)
                                       (error logger 
                                              "msg/decrypt!: Exception occured while attempting to decrypt the given data."
                                              e)
                                       #f)))
                 (cipher-update! (session-ictx session) data))))
          (if (boolean? decrypted-msg)
              (begin
                (debug logger "msg/decrypt!: Failed to decrypt the data.")
                #f)
              ; Decryption succeeded.
              (letrec ((cipher-type (session-cipher-type session))
                       (digest-type (session-digest-type session))
                       (key (session-key session))
                       (block-size (cipher-block-size cipher-type))
                       
                       ; Decrypt the data, then remove the padding.
                       (msg (remove-padding decrypted-msg block-size))
                       
                       ; Extract the bytes belonging to the hmac.
                       (dsize (digest-size digest-type))
                       (mac (subbytes msg 0 dsize))
                       
                       ; Extract the mac-data.
                       (mac-data (subbytes msg dsize))
                       
                       ; Extract the message length.
                       (length-bytes (subbytes mac-data 0 (message-length-in-bytes)))
                       (length (bytes->msg-length length-bytes))
                       
                       ; Extract the actual message.
                       (msgout (subbytes mac-data (message-length-in-bytes))))
                (cond
                  ; Compute a new mac and make sure it is what was sent and compare the
                  ; message lengths.
                  [(and
                    ; Verify that the mac is valid.
                    (bytes=? mac (hmac digest-type key mac-data))
                    ; Verify that the length is valid.
                    (= length (bytes-length msgout)))
                   msgout]
                  [else
                   (debug logger "msg/decrypt!: The mac or length did not match the computed values.")
                   #f]))))))
  (provide/contract
   [msg/decrypt! (-> session? bytes? (or/c bytes? boolean?))])
  
  ; The automatic padding didn't work when trying to pass multiple messages
  ; through the ciphers.  The ciphers would get confused and not always pad
  ; data correctly.  I have implemented one of the suggested padding methods
  ; by Schneier and Ferguson.
  ; "Pad the last block with n bytes all with value n."
  ;  - http://en.wikipedia.org/wiki/Block_cipher_modes_of_operation
  ;    First paragraph of the Padding section.
  (define (add-padding ptext block-size)
    ; Get the number of bytes needed for padding.
    (let ((pad (get-bytes ptext block-size)))
      (if (= pad 0)
          ; Don't need padding, but we need to add another full block.
          (bytes-append ptext (make-bytes block-size block-size))
          ; Pad N bytes with the value N
          (bytes-append ptext (make-bytes pad pad)))))
  (provide/contract
   [add-padding (-> bytes? exact-integer? bytes?)])
  
  ; Returns the number of bytes that are needed to pad the given
  ; data byte string to a nice boundary based upon the given
  ; block-size.
  (define (get-bytes data block-size)
    (let ((num-blocks (ceiling (/ (bytes-length data) block-size))))
      (- (* num-blocks block-size) (bytes-length data))))
  (provide/contract
   [get-bytes (-> bytes? exact-integer? exact-integer?)])
  
  ; Returns the data byte string with all padding values removed.
  ; See add-padding for a reference to which type of padding we add.
  (define (remove-padding data block-size)
    (letrec ((length (bytes-length data))
             ; Convert the last byte value into a value.
             ; This value will tell us the amount of padding
             ; as well as the values for the padding.
             (num (integer-bytes->integer
                   (bytes-append (make-bytes 1 0) (subbytes data (- length 1)))
                   #f
                   #t)))
      (if (> num block-size)
          ; If the value is larger then the block-size then something went wrong.
          ; Let's just return the data.
          (begin
            (error logger "remove-padding: The padding value was greater than the block-size.  This should never occur.")
            data)
          ; We padded the data, grab the last num subbytes of the data
          ; message and make sure that they are all of the same value.
          (let ((pad (subbytes data (- length num))))
            (if (bytes=? pad (make-bytes num num))
                ; Found our padding, return the valid message.
                (subbytes data 0 (- length num))
                (begin
                  (error logger "remove-padding: The padding did not match the expected byte-string value.")
                  data))))))
  (provide/contract
   [remove-padding (-> bytes? exact-integer? bytes?)])
  
  ; Returns the length of the message in bytes.  The number of bytes will be
  ; equal to the defined +message-length-in-bytes+ value.
  (define (msg-length msg)
    (integer->integer-bytes (bytes-length msg) (message-length-in-bytes) #f #t))
  (provide/contract
   [msg-length (-> bytes? bytes?)])
  
  ; Returns given bytes as an exact-integer.
  (define (bytes->msg-length l)
    (integer-bytes->integer l #f #t))
  (provide/contract
   [bytes->msg-length (-> bytes? exact-integer?)])

