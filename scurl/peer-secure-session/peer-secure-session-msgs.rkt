#lang racket
  ; This module provides the message definitions for the peer-secure-session protocol
  ; and creation and accessor functions for those messages.
  
  (require "depends.rkt"
           "crypto.rkt"
           
           "../peer-validation/peer-validation-msgs.rkt")
  
  ; Create a logger for this module.
  (define logger (make-logger 'peer-secure-session-msgs-logger peer-secure-session-parent-logger))
  
  ; Message id and message for the secure connection options request message.
  ; This message is sent to initiate the shared key dance between a remote peer.
  (define +secure-connection-options-request-msg-id+ 5003)
  (define (secure-connection-options-request-msg)
    (list +secure-connection-options-request-msg-id+))
  (provide/contract
   [secure-connection-options-request-msg (-> list?)])
  
  ; Returns true when the given list is a valid secure-connection-options-request msg,
  ; false otherwise.
  (define (secure-connection-options-request-msg? msg)
    (if (and
         (list? msg)
         (= (length msg) 1))
        (equal? (first msg) +secure-connection-options-request-msg-id+)
        #f))
  (provide/contract
   [secure-connection-options-request-msg? (-> any/c boolean?)])
  
  ; Message id and message for the secure connection options response message.
  ; Returns a list of ciphers and digests that the peer is allowed to choose
  ; from to create a shared key session.
  (define +secure-connection-options-response-msg-id+ 6003)
  (define (secure-connection-options-response-msg)
    (list +secure-connection-options-response-msg-id+
          CIPHER-IDS))
  (provide/contract
   [secure-connection-options-response-msg (-> list?)])
  
  ; Returns true when the given list is a valid secure-connection-options-response-msg,
  ; false otherwise.
  (define (secure-connection-options-response-msg? msg)
    (if (and
         (list? msg)
         (= (length msg) 2))
        (let ((msg-id (first msg))
              (cipher-ids (second msg)))
          (and
           (equal? msg-id +secure-connection-options-response-msg-id+)
           (list? cipher-ids)
           (andmap exact-positive-integer? cipher-ids)))
        (begin
          (debug logger "secure-connection-options-response-msg?: Not enough elements in the given list.")
          #f)))
  (provide/contract
   [secure-connection-options-response-msg? (-> any/c boolean?)])
  
  ; Returns the list of ciphers from the message if possible, false otherwise.
  (define (secure-connection-options-response-msg-ciphers msg)
    (if (secure-connection-options-response-msg? msg)
        (integers->ciphers (second msg))
        #f))
  (provide/contract
   [secure-connection-options-response-msg-ciphers (-> any/c (or/c boolean? (listof (or/c boolean? !cipher?))))])
  
  ; Message id and message for the secure connection request message.
  ; This message is sent from the initiating peer once it has choosen
  ; a valid cipher and digest type.  The shared-keys
  ; field will contain the generated key halves from the initiating peer
  ; that are used to create a shared secret and the cipher and digest
  ; field will contain the cipher type and digest type that the peer
  ; has choosen to use.
  (define +secure-connection-request-msg-id+ 5004)
  
  ; Returns a secure-connection-request-msg with the choosen cipher type and digest type.
  ; The shared-keys that are generated will be based off of the choosen cipher type.
  (define (secure-connection-request-msg cipher)
    ; Generate the shared keys and return the message.
    (let ((keys (generate-shared-keys cipher)))
      (list +secure-connection-request-msg-id+
            (cipher->integer cipher)
            (shared-keys-kcs keys)
            (shared-keys-ivcs keys)
            (shared-keys-ksc keys)
            (shared-keys-ivsc keys))))
  (provide/contract
   [secure-connection-request-msg (-> !cipher? list?)])
  
  ; Returns true when the given list is defined by the correct message id, false otherwise.
  (define (secure-connection-request-msg? msg)
    (if (and
         (list? msg)
         (= (length msg) 6))
        (let ((msg-id (first msg))
              (cipher-id (second msg))
              (kcs (third msg))
              (ivcs (fourth msg))
              (ksc (fifth msg))
              (ivsc (sixth msg)))
          (and
           (equal? msg-id +secure-connection-request-msg-id+)
           (exact-positive-integer? cipher-id)
           (bytes? kcs)
           (bytes? ivcs)
           (bytes? ksc)
           (bytes? ivsc)))
        (begin
          (debug logger "secure-connection-request-msg?: Given list is not valid.")
          #f)))
  (provide/contract
   [secure-connection-request-msg? (-> any/c boolean?)])
  
  ; Returns the cipher in the secure-connection-request-msg given, otherwise
  ; returns false if the message is not valid.
  (define (secure-connection-request-msg-cipher msg)
    (if (secure-connection-request-msg? msg)
        (integer->cipher (second msg))
        #f))
  (provide/contract
   [secure-connection-request-msg-cipher (-> any/c (or/c boolean? !cipher?))])
  
  ; Returns the shared-keys in the secure-connection-request-msg given, otherwise
  ; returns false if the message is not valid.
  (define (secure-connection-request-msg-shared-keys msg)
    (if (secure-connection-request-msg? msg)
        (let ((kcs (third msg))
              (ivcs (fourth msg))
              (ksc (fifth msg))
              (ivsc (sixth msg)))
          (shared-keys kcs ivcs ksc ivsc))
        #f))
  (provide/contract
   [secure-connection-request-msg-shared-keys (-> any/c (or/c boolean? shared-keys?))])
  
  ; Message id and message for the secure connection response message.
  ; This message is sent in response to the initiating peer's request to
  ; establish a secure channel (in the form of a secure connection request
  ; message).  The shared-keys field will contain the generated key halves
  ; from the remote peer that are used to create a shared secret.
  (define +secure-connection-response-msg-id+ 6004)
  
  ; Returns a secure-connection-response-msg that contains the cipher and digest
  ; type given as well as a generated shared-keys structure based upon the cipher
  ; type.
  (define (secure-connection-response-msg cipher)
    ; Create the shared-keys based upon the cipher type.
    (let ((keys (generate-shared-keys cipher)))
      (list +secure-connection-response-msg-id+
            (shared-keys-kcs keys)
            (shared-keys-ivcs keys)
            (shared-keys-ksc keys)
            (shared-keys-ivsc keys))))
  (provide/contract
   [secure-connection-response-msg (-> !cipher? list?)])
  
  ; Returns true when the given item is a valid secure-connection-response-msg,
  ; false otherwise.
  (define (secure-connection-response-msg? msg)
    (if (and
         (list? msg)
         (= (length msg) 5))
        (let ((msg-id (first msg))
              (kcs (second msg))
              (ivcs (third msg))
              (ksc (fourth msg))
              (ivsc (fifth msg)))
          (and
           (equal? msg-id +secure-connection-response-msg-id+)
           (bytes? kcs)
           (bytes? ivcs)
           (bytes? ksc)
           (bytes? ivsc)))
        (begin
          (debug logger "secure-connection-response-msg?: The given item is not a valid message.")
          #f)))
  (provide/contract
   [secure-connection-response-msg? (-> any/c boolean?)])
  
  ; Returns the shared-keys object when given a valid secure-connection-response-msg,
  ; false otherwise.
  (define (secure-connection-response-msg-shared-keys msg)
    (if (secure-connection-response-msg? msg)
        (shared-keys
         (second msg)
         (third msg)
         (fourth msg)
         (fifth msg))
        #f))
  (provide/contract
   [secure-connection-response-msg-shared-keys (-> any/c (or/c boolean? shared-keys?))])

