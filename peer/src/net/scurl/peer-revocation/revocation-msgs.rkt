#lang racket/base

(require racket/contract racket/list)
  
  (require "depends.rkt"
           "revo-cert.rkt"
           "../peer-validation/scurl.rkt"
           "../peer-validation/peer-validation-msgs.rkt")
  
  (provide key-revocation-response-msg
           key-revocation-response-msg?
           key-revocation-response-msg-revo-cert)
  
  ; Create the logger for the revocation-msgs module.
  (define logger (make-logger 'revocation-msgs-logger revocation-parent-logger))
  
  ; Message id and message for the key revocation certificate message.
  ; This message is used in response to a key-request-msg to inform the
  ; peer that the requested scurl has been compromised and should no
  ; longer be trusted.
  (define +key-revocation-response-msg-id+ 7001)
  
  ; Returns a key-revocation-response-msg that contains the scurl information
  ; and a signature using the host-id as the input.
  ; Returns a list with id (int), scurl (string), digest (int),
  ; pkey (int), key (bytes, and signature (bytes)
  ;
  ; revo-cert? -> list?
  (define (key-revocation-response-msg revo-cert)
    (let ((scurl (revo-cert-scurl revo-cert))
          (sig (revo-cert-signature revo-cert)))
      (list +key-revocation-response-msg-id+
            (url->string (scurl-url scurl))
            (digest->integer (scurl-digest-type scurl))
            (pkey->integer (scurl-key-type scurl))
            (public-key->bytes (scurl-key scurl))
            sig)))
  
  ; Returns true when the given item is a list with the proper message id
  ; and proper element types.
  ;
  ; any -> boolean?
  (define (key-revocation-response-msg? msg)
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
           (equal? msg-id +key-revocation-response-msg-id+)
           (string? url)
           (exact-positive-integer? digest-type-id)
           (exact-positive-integer? pkey-type-id)
           (bytes? key)
           (bytes? sig)))
        ; Not enough elements.
        #f))
  
  ; Extracts the revo-cert components from the given list which should have
  ; been created from the key-revocation-response-msg function and creates a revo-cert
  ; from them.
  ; Returns false if the revo-cert cannot be created.
  ;
  ; any -> (or boolean? revo-cert?)
  (define (key-revocation-response-msg-revo-cert msg)
    ; Verify that we have the correct message first.
    (if (key-revocation-response-msg? msg)
        ; Extract the items and perform conversions to types.
        (let ((url (second msg))
              (digest-type (integer->digest (third msg)))
              (key-type (integer->pkey (fourth msg)))
              (key-bytes (fifth msg))
              (sig (sixth msg)))
          ; Make sure all of the conversions went smoothly.
          (if (and
               (!digest? digest-type)
               (!pkey? key-type))
              ; Attempt to create a pkey object.
              (let ((key (with-handlers
                             ((exn? (lambda (e)
                                      (debug logger 
                                             "Failed to create a pkey out of the key-revocation-response-msg."
                                             e)
                                      #f)))
                           (bytes->public-key key-type key-bytes))))
                (if (pkey? key)
                    ; Let's try to create a scurl.
                    (let ((scurl (string->scurl url digest-type key-type key)))
                      (if (scurl? scurl)
                          (revo-cert scurl sig)
                          (begin
                            (debug logger "Failed to create a scurl from the key-revocation-response-msg.")
                            #f)))
                    (begin
                      (debug logger "Failed to create a pkey out of the key-revocation-response-msg.")
                      #f)))
              (begin
                (debug logger "The digest-type-id or pkey-type-id failed to convert to a valid type.")
                #f)))
        (begin
          (debug logger "The given item was not a valid key-revocation-response-msg.")
          #f)))

