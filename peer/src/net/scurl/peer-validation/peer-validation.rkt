#lang racket
; The peer-validation module contains the message protocol for validating two
; peers using scurls.  It also contains methods for validating the messages
; passed between peers.  The two methods of interest for any user would be
; "request-established-connection" and "respond-established-connection".

(require "depends.rkt"
         "scurl.rkt"
         "peer-validation-msgs.rkt")

(provide
 handle-client-authentication
 handle-server-authentication
 
 ; Private
 authenticate-client-auth-msg?
 authenticate-server-auth-msg?
 create-signature
 generate-nonce
 generate-version-nonce
 verify-signature
 )

; Create a logger for this module.
(define logger (make-logger 'peer-validation-logger peer-validation-parent-logger))

(define error* (curry error logger))
(define warning* (curry warning logger))
(define debug* (curry debug logger))

; The maximum version allowable in this implementation.
(define +max-version+ 1.0)

; The version number for version 1.0
(define +version-v1+ 1.0)

; The length of the nonce for version 1.0.
(define +nonce-length-v1+ 28)

; The amount of delta allowed between the received time and current time
; when checking acceptable timestamps in seconds.
(define +allowable-timestamp-delta+ 10)

(define client-authfun/c 
  (private-scurl? scurl? input-port? output-port? server-hello-msg?
                  . -> . (or/c #f scurl?)))
(define server-authfun/c 
  (private-scurl? (scurl? . -> . boolean?) input-port? output-port? exact-positive-integer? bytes?
                  . -> . (or/c #f scurl?)))

; The handle-server-authentication function takes four parameters.
; local-scurl - the scurl describing the local peer.
; handle-revocation - a function which takes a single argument.  The argument is the
;             scurl retrieved from the remote peer and should be used to perform
;             revocation on the connecting peer.  The function should return a
;             boolean value of true to indicate the the connection process should
;             NOT proceed and that the peer has been revoked.
; in - an input-port used to read incoming messages from the remote peer.
; out - an output-port used to write outgoing messages to the remote peer.
;
; If the protocol succeeds then a scurl will be returned, otherwise false.
; Anything other than a scurl should signify a failure of the protocol.
(define/contract (handle-server-authentication local-scurl handle-revocation in out)
  (private-scurl? (scurl? . -> . boolean?) input-port? output-port? . -> . (or/c #f scurl?))
  ; Catch all handler, something we did not expect happened, return false.
  (with-handlers 
      ([exn? (λ (e)
               (error* "Exception occured while attempting to validate remote peer's incoming request." e)
               #f)])
    
    ; Read the client-hello-msg from the input port.
    (define msg (read in))
    ; Validate the message type.
    (cond
      [(not (client-hello-msg? msg)) ; Don't know what we got, return false.
       (debug* "Failed to receive a client-hello-msg.") #f]
      [else
       ; Extract the max version number.
       (define c-max-version (client-hello-msg-max-version msg))
       ; If the given version is greater than our max,
       ; use our max, otherwise use the given version.
       (define version (if (> c-max-version +max-version+) +max-version+ c-max-version))
       ; Generate a timestamp and nonce for the client to sign with.
       (define s-time (current-seconds))
       (define s-nonce (generate-version-nonce version))
       
       ; Now, write the server-hello-msg.
       (write (server-hello-msg version s-time s-nonce) out)
       (flush-output out)
       ; Check the version number and call the function implementing the correct protocol.
       (cond
         [(= version +version-v1+) 
          (handle-server-authentication-v1 local-scurl handle-revocation in out s-time s-nonce)]
         [else ; We don't support this version.
          (debug* "Failed to identify the requested version.") #f])])))

(define/contract (handle-server-authentication-v1 local-scurl handle-revocation in out s-time s-nonce)
  server-authfun/c
  ; Read the client-auth-msg.
  (define msg (read in))
  
  (cond 
    [(not (client-auth-msg? msg))
     (debug* "Failed to receive a client-auth-msg.") #f]
    ; Authenticate the client. Authenticate that the peer
    ; can prove that they own the private key based off of the signature.
    [(not (authenticate-client-auth-msg? s-time s-nonce msg))
     (debug* "Failed to authenticate the peer's client-auth-msg.") #f]
    
    [else
     ; We know the peer owns the public/private key, but let's run revocation
     ; to see if they are allowed to connect.  This would be the part where
     ; the revocation service would check the retrieved scurl's host-id against
     ; a known host-id for the same scurl.
     (define retrieved-scurl (client-auth-msg-scurl msg))
     (define c-time (client-auth-msg-timestamp msg))
     (define c-nonce (client-auth-msg-nonce msg))
     
     ; We must have a real scurl and the client timestamp must be within a certain delta
     ; of our time or we will reject it.
     (cond
       [(or (not (scurl? retrieved-scurl))
            (not (<= (abs (- c-time (current-seconds))) +allowable-timestamp-delta+)))
        (debug* "Failed to extract a valid scurl from the key-request-msg.") #f]
       
       [(run-revocation-handler handle-revocation retrieved-scurl)
        ; Failed to pass the revocation handler.
        (debug* "The connecting peer was revoked.") #f]
       
       [else                         
        ; Generate a signature and send the server-auth-msg with our scurl information.
        (define sig (create-signature local-scurl c-time c-nonce))
        (write (server-auth-msg local-scurl sig) out)
        (flush-output out)
        
        ; Read the client-done-msg which should contain whether
        ; we have been authenticated to continue communication.
        ; If this isn't a client-done-msg it probably is an eof
        ; or something else and the connection is not going to be allowed.
        (define msg2 (read in))
        (cond
          [(not (client-done-msg? msg2))
           (debug* "Failed to receive a client-done-msg.") #f]
          [(not (client-done-msg-is-authenticated msg2))
           (debug* "The client-done-msg returned, but did not have a true value for the is-valid field.")
           #f]
          [else #|Success, return the retrieved scurl.|# retrieved-scurl])])]))

(define/contract (run-revocation-handler handle-revocation remote-scurl)
  ((scurl? . -> . boolean?) scurl? . -> . boolean?)
  (with-handlers ([exn? (λ (e)
                          (warning* (string-append "Exception occured during revocation for "
                                                   (url->string (scurl-url remote-scurl)))
                                    e)
                          ; Revocation function failed to complete, so we will revoke.
                          #t)])
    (handle-revocation remote-scurl)))

; The handle-client-authentication function takes four parameters.
; local-scurl - the scurl describing the local peer.
; remote-scurl - the scurl or url describing the remote peer.
; in - an input-port used to read incoming messages from the remote peer.
; out - an output-port used to write outgoing messages to the remote peer.
;
; If the protocol succeeds then a scurl will be returned, otherwise false.
; Anything other than a scurl should signify a failure of the protocol.
(define/contract (handle-client-authentication local-scurl remote-scurl* in out)
  (private-scurl? (or/c scurl? url?) input-port? output-port? . -> . (or/c #f scurl?))
  ; Catch all handler, something we did not expect happened, return false.
  (with-handlers ((exn? (λ (e)
                          (warning* 
                           "Exception occured while attempting to validate local peer's outgoing request."
                           e)
                          #f)))
    
    ; Convert the given remote-scurl to a scurl if it is an url.
    (define remote-scurl (if (url? remote-scurl*) (url->scurl remote-scurl*) remote-scurl*))
    
    ; Send the client hello message.
    (write (client-hello-msg +max-version+) out)
    (flush-output out)
    
    ; Read the server hello message.
    (define msg (read in))
    ; Make sure we got a valid hello message.
    (cond
      [(not (server-hello-msg? msg)) ; Don't know what we got. Fail.
       (debug* "Failed to receive a server-hello-msg.") #f]
      [(equal? (server-hello-msg-version msg) +version-v1+)
       ; Extract the chosen version and call the correct version impl function.
       (handle-client-authentication-v1 local-scurl remote-scurl in out msg)]
      [else ; What the heck version is that?
       (debug* "Failed to understand the server-hello-msg version number.") #f])))

; Performs the remainder of the scurl authentication protocol for version v1.
; This function should only be called internally.
(define/contract (handle-client-authentication-v1 local-scurl remote-scurl in out msg)
  client-authfun/c
  ; Extract the server info, use the server's timestamp/nonce to create the client's signature.
  ; Generate a timestamp/nonce for the server to use.
  (define s-time (server-hello-msg-timestamp msg))
  (define s-nonce (server-hello-msg-nonce msg))
  
  ; The server timestamp must be within a certain delta
  ; of our time or we will reject it.
  (cond
    [(not (<= (abs (- s-time (current-seconds))) +allowable-timestamp-delta+))
     ; Bad time - out of range
     (debug* "The timestamp from the server was out of our delta range.") #f]
    [else
     ; Time looks good, continue.
     (define c-time (current-seconds))
     (define c-nonce (generate-version-nonce +nonce-length-v1+))
     (define c-sig (create-signature local-scurl s-time s-nonce))
     
     ; Write the client authenticate message.
     (write (client-auth-msg local-scurl c-time c-nonce c-sig) out)
     (flush-output out)
     
     ; Read the server authenticate message.
     (define msg (read in))
     (cond
       ; Make sure we got a valid server-auth-msg.
       [(not (server-auth-msg? msg)) ; Don't know what we got, but let's return it.
        (debug* "Failed to recieve a server-auth-msg.") #f]
       
       ; Good message, let's authenticate them.
       [(not (authenticate-server-auth-msg? remote-scurl c-time c-nonce msg))
        (debug* "Failed to authenticate the remote server.") #f] 
       
       [else
        ; The server is authenticated, let's return the client done message.
        (write (client-done-msg #t) out)
        (flush-output out)
        ; Return the remote retrieved scurl to indicate authentication successfull.
        (server-auth-msg-scurl msg)])]))

; This function uses the known-scurl from the local peer and the server-auth-msg
; from the remote peer to fully validate the remote peer's identity.  Because we
; know the scurl of the peer we attempted to contact we can authenticate both that
; the peer is who they claim to be and that they are who we expect them to be.
; If the peer is valid then the return value will be true and if the peer is invalid
; then the return value will be false.
(define/contract (authenticate-server-auth-msg? known-scurl timestamp nonce msg)
  (scurl? exact-positive-integer? bytes? server-auth-msg? . -> . boolean?)
  (define retrieved-scurl (server-auth-msg-scurl msg))
  (define sig (server-auth-msg-signature msg))
  (define (scurl->host-id s)
    (make-host-id (scurl-url s) (scurl-digest-type s) (scurl-key s)))
  ; Steps to authenticate a remote peer.
  ; 1. Grab the host-id from the known-scurl.
  ; 2. Create the host-id using info from the retrieved-scurl.
  ; 3. If the host-id's match then we know that the retrieved-scurl has the public key,
  ;    location and port that we expect from the known-scurl.
  ; Step 3 has successfully authenticated that the remote peer is identifing itself as
  ; whom we expected them to.
  ; 4. Verify the signature which will prove that the remote peer owns the private key
  ;    that correlates with the public key.  It is safe to use the retrieved scurl
  ;    info to perform verification because we proved they are using info we expect in
  ;    Step 3.
  ; Step 4 has successfully authenticated that the remote peer is who they say they are.
  
  ; We must have a real scurl.
  (and (scurl? retrieved-scurl)         
       ; Grab the host-id from the known-scurl and make the host-id for the retrieved scurl.
       (let ([known-host-id (scurl-host-id known-scurl)]
             [retrieved-host-id (scurl->host-id retrieved-scurl)])
         ; Compare the host-id's to validate the retrieved information.
         (and (bytes=? known-host-id retrieved-host-id)
              ; Success, now check the signature.
              (verify-signature retrieved-scurl timestamp nonce sig)))))

; This function uses the server generated timestamp and nonce along with the retrieved
; scurl and signature from the remote peer to authenticate the remote peer's identity.
; This authentication can only prove that the remote peer can prove that they own the
; private key that correlates to the public key.  That means they are who they claim to
; be.
; If the peer is authenticated then the return value will be true and if the peer is
; fails authentication then the return value will be false.
(define/contract (authenticate-client-auth-msg? timestamp nonce msg)
  (exact-positive-integer? bytes? client-auth-msg? . -> . boolean?)
  ; The message must yield a scurl.
  (and (scurl? (client-auth-msg-scurl msg))
       (verify-signature (client-auth-msg-scurl msg) timestamp nonce (client-auth-msg-signature msg))))

; Creates a signature given the scurl, timestamp and nonce.
(define/contract (create-signature scurl timestamp nonce)
  (scurl? exact-positive-integer? bytes? . -> . bytes?)
  ; Append the timestamp in bytes and nonce to the host-id to form
  ; the base of the signature.
  (define base (bytes-append (scurl-host-id scurl) (string->bytes/utf-8 (number->string timestamp)) nonce))
  (sign (scurl-key scurl) (signature-digest) base))

; Verifies a signature given the scurl, timestamp, nonce and signature.
(define/contract (verify-signature scurl timestamp nonce signature)
  (scurl? exact-positive-integer? bytes? bytes? . -> . boolean?)
  (define base (bytes-append (scurl-host-id scurl) (string->bytes/utf-8 (number->string timestamp)) nonce))
  (verify (scurl-key scurl) (signature-digest) signature base))

; Generate a nonce based upon the version given.
(define/contract (generate-version-nonce version)
  (number? . -> . bytes?)
  (cond
    [(= version +version-v1+) (generate-nonce +nonce-length-v1+)]
    [else
     ; Don't recognize the version number, doesn't matter as we'll
     ; bail when attempting to find the function to call when handling
     ; the rest of the protocol.
     (generate-nonce +nonce-length-v1+)]))

; Generate a random byte string of the specified length
(define/contract (generate-nonce length)
  (number? . -> . bytes?)
  (define bts (make-bytes length 0))
  (for ([i (in-range length)])
    (bytes-set! bts i (random 255)))
  bts)