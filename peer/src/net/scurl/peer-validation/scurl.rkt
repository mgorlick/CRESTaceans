#lang racket
  ; Provides the scurl structure and functions for scurl creation and scurl validation.
  ; As well as functions to generate the host-id for a scurl.
  
  (require "depends.rkt"
           "host-id.rkt")
  
  (provide make-host-id-base)
  
  ; Create the logger for the scurl module.
  (define logger (make-logger 'scurl-logger peer-validation-parent-logger))
  
  ; A scurl holds all of the information necessary to validate
  ; a remote peer.  The digest-type, key-type and key are allowed to be
  ; null because it is still possible to validate a remote peer if all that
  ; is known is the url and host-id.
  (struct scurl (url host-id digest-type key-type key) #:transparent)
  (provide/contract
   [struct scurl ((url url?) ; url with the string encoded host-id.
                  (host-id bytes?) ; the byte encoded host-id.
                  (digest-type (or/c null? !digest?)) ; the digest used to create the host-id.
                  (key-type (or/c null? !pkey?)) ; the type of pkey that the key object is.
                  (key (or/c null? pkey?)) ; the pkey used to sign/verify the host-id.
                  )])
  
  ; Returns true when the scurl contains a pkey that contains a private
  ; key component.
  (define (private-scurl? s)
    (and (scurl? s)
         (pkey-private? (scurl-key s))))
  (provide/contract
   [private-scurl? (-> any/c boolean?)])
  
  ; Returns true when the digest-type, key-type or key of the given scurl
  ; is null.
  (define (null-scurl? s)
    (and (scurl? s)
         (or (null? (scurl-digest-type s))
             (null? (scurl-key-type s))
             (null? (scurl-key s)))))
  (provide/contract
   [null-scurl? (-> any/c boolean?)])
  
  ; Returns true when two scurls are equivalent to each other.
  ; The comparison is done based upon the string version of the scurl.
  (define (scurl=? s1 s2)
    ; Convert both scurls to string and perform a string comparison.
    (and (scurl? s1)
         (scurl? s2)
         (string=? (url->string (scurl-url s1))
                   (url->string (scurl-url s2)))))
  (provide/contract
   [scurl=? (-> any/c any/c boolean?)])
  
  ; Searches for the host-id given a list of path/param objects
  ; that have been retrieved by parsing a url.  If the string
  ; "scurl" is found as part of the path then the next path value
  ; will be taken to be the host-id
  (define (find-host-id path)
    (cond
      [(null? path)
       (debug logger "Failed to find a scurl path element while searching for the host-id in an url.")
       #f]
      [else
       ; If we found the text "scurl" and we have another path element
       ; then hopefully it is the host-id
       (if (and (string=? (path/param-path (first path)) "scurl")
                (> (length (rest path)) 0))
           (let ((host-id-text (path/param-path (second path))))
             ; If the host-id-text is a valid length to be the host-id in string
             ; form then let's convert it to byte form, otherwise return false.
             (if (host-id-string? host-id-text)
                 (host-id-string->host-id-bytes host-id-text)
                 (begin
                   (debug logger "Found a scurl path element, but the following element was not a valid host-id in string form.")
                   #f)))
           (find-host-id (cdr path)))]))
  (provide/contract
   [find-host-id (-> (listof path/param?) (or/c bytes? boolean?))])
  
  ; This function converts a string into a SCURL.  If the given string
  ; can be parsed correctly then the scurl is returned, otherwise a
  ; boolean value of false is returned.
  (define (string->scurl text [digest-type null] [key-type null] [key null])
    (url->scurl (string->url text) digest-type key-type key))
  (provide/contract
   [string->scurl (->* (string?) (!digest? !pkey? (or/c pkey? bytes?)) (or/c scurl? boolean?))])
  
  ; This function converts an URL into a SCURL.  If the
  ; given URL is formatted correctly then the scurl is
  ; returned, otherwise a boolean value of false is returned.
  (define (url->scurl url [digest-type null] [key-type null] [key null])
    (let (; Extract a valid key.
          (valid-key
           (cond
             ; Key is already a pkey, pass it along.
             [(pkey? key) key]
             ; Key is a byte-string and key-type is not null,
             ; let's try to turn it into a pkey.
             [(and (bytes? key)
                   (!pkey? key-type))
              (with-handlers ((exn? (lambda (e)
                                      (debug logger "Failed to convert the given bytes and pkey type into a pkey." e)
                                      null)))
                (bytes->public-key key-type key))]
             [else
              ; Return null, because key could be bytes,
              ; but not a valid bytes to make a pkey.
              null])))
      ; Find the host-id in the path portion of the url.
      (let ((host-id (find-host-id (url-path url))))
        (if (host-id-bytes? host-id)
            ; Found a valid host-id, create a scurl.
            (scurl url host-id digest-type key-type valid-key)
            ; Couldn't find a host-id, so fail to create a scurl.
            (begin
              (debug logger "Failed to find a valid host-id!")
              #f)))))
  (provide/contract
   [url->scurl (->* (url?) (!digest? !pkey? (or/c pkey? bytes?)) (or/c scurl? boolean?))])
  
  ; Returns the string representation of the scurl.
  (define (scurl->string s)
    (url->string (scurl-url s)))
  (provide/contract
   [scurl->string (-> scurl? string?)])
  
  ; Turns the given scurl into a scurl structure that contains only the
  ; public-key portion of the pkey.  The output of this function is safe
  ; to give out to the public.
  (define (scurl->public-scurl s)
    (scurl (scurl-url s)
           (scurl-host-id s)
           (scurl-digest-type s)
           (scurl-key-type s)
           (pkey->public-key (scurl-key s))))
  (provide/contract
   [scurl->public-scurl (-> scurl? scurl?)])
  
  ; Takes an url that has no scurl tag or host-id and generates a scurl
  ; structure that contains the scurl that is associated with the given url
  ; and key information.
  (define (generate-scurl text digest-type key-type key)
    ; Convert the text string into a url.
    (letrec ((url (if (string? text)
                      ; Convert from string to url.
                      (string->url text)
                      ; Must be an url already.
                      text))
             ; Now, generate a new host-id based on the given key.
             (host-id (make-host-id text digest-type key)))
      ; Return a valid scurl structure that has the correct information.
      (scurl
       ; Inject the host-id as a string into the url.
       (insert-host-id url (host-id-bytes->host-id-string host-id))
       host-id
       digest-type
       key-type
       key)))
  (provide/contract
   [generate-scurl (-> (or/c string? url?) !digest? !pkey? pkey? scurl?)])
  
  ; Inserts a host-id that is in string format into the path/param list
  ; of the given url.
  (define (insert-host-id url host-id)
    (let ((path (url-path url))
          (scurl-path (list (make-path/param "scurl" empty)
                            (make-path/param host-id empty))))
      (let ((full-path
             (if (empty? path)
                 ; If path/param is empty, we need to add a blank path/param
                 ; to get the '/' at the beginning of the url.
                 (append (list (make-path/param "" empty))
                         scurl-path)
                 (append scurl-path
                         path))))
        (make-url (url-scheme url)
                  (url-user url)
                  (url-host url)
                  (url-port url)
                  (url-path-absolute? url)
                  full-path
                  (url-query url)
                  (url-fragment url)))))
  (provide/contract
   [insert-host-id (-> url? host-id-string? url?)])
  
  ; This function computes the host-id of a scurl structure.
  ;   SHA( SHA(location:port:publickey):location:port:publickey)
  (define (make-host-id url digest-type key)
    (let ((data (make-host-id-base url key)))
      ; Produce the host id by doing DIGEST( DIGEST(data):data ) and return the number of bytes needed.
      (subbytes
       (digest digest-type
               (bytes-append (digest digest-type data) data))
       ; We only the need a certain number of bytes.
       0
       (host-id-bytes-length))))
  (provide/contract
   [make-host-id (-> (or/c string? url?) !digest? pkey? host-id-bytes?)])
  
  ; This function creates the byte-string that is used to create the host-id of a scurl.
  ;
  ; (or url? string?) pkey? -> byte-string?
  (define (make-host-id-base url key)
    (let* (; Convert the location, port and public key into bytes.
           (url (if (string? url) (string->url url) url))
           (location-bytes (string->bytes/utf-8 (url-host url)))
           ; Default the port value to 8080 if there is none present.
           (port (if (boolean? (url-port url))
                     8080
                     (url-port url)))
           (port-bytes (integer->integer-bytes port 2 #f #t))
           
           ; Extract the bytes that represent the public key portion.
           (key-bytes (public-key->bytes key)))
      
      ; Append the location, port and public key into one byte string.
      (bytes-append location-bytes port-bytes key-bytes)))
  
  ; Find the scurl tag and remove it and the following path/param element
  ; so that the final url contains no scurl or host id.
  (define (remove-host-id path)
    (cond
      ; If null return empty list.
      [(null? path) '()]
      ; If the path is a scurl tag try to remove it and the host id.  Stop searching after this.
      [(string=? (path/param-path (car path)) "scurl")
       (let (; Remove the scurl tag from the list.
             (path (cdr path)))
         ; We should have another element, the host-id.
         (if (> (length path) 0)
             ; If we do, remove it.
             (cdr path)
             ; Don't know how that happened, but let's
             ; just return the path without the scurl tag.
             path))]
      [else
       (append (car path) (remove-host-id (cdr path)))]))
  
  ; Returns a url that represents the given scurl without the scurl and
  ; host-id path elements.
  (define (scurl->url-without-host-id scurl)
    (let ((url (scurl-url scurl)))
      (make-url (url-scheme url)
                (url-user url)
                (url-host url)
                (url-port url)
                (url-path-absolute? url)
                (remove-host-id (url-path url))
                (url-query url)
                (url-fragment url))))
  (provide/contract
   [scurl->url-without-host-id (-> scurl? url?)])

