#lang racket
  
  (require "depends.rkt"
           "../peer-validation/scurl.rkt")
  
  (provide revocation-signature-digest
           (struct-out revo-cert)
           revo-cert=?
           
           scurl->revo-cert
           valid-revo-cert?)
  
  ; Create the logger for the revocation module.
  (define logger (make-logger 'revocation-logger revocation-parent-logger))
  
  ; The digest algorithm that is used to sign and verify the revocation certificate.
  (define +revocation-signature-digest+ digest:sha256)
  (define (revocation-signature-digest)
    +revocation-signature-digest+)
  
  ; The string that is prepended to the host-id bytes before making a signature.
  (define +revocation-signature-prefix+ "PathRevoke")
  
  ; The revo-cert represents a scurl whose private key component has been
  ; compromised.  The signature is present to prove that the revo-cert is valid
  ; and is created by hashing the location, port, public key and the string
  ; constant "PathRevoke".  The string constant "PathRevoke" is added to differentiate
  ; this signature from the signature used to authenticate scurls.
  ;
  ; scurl? bytes?
  (struct revo-cert (scurl signature))
  
  ; Returns true when both arguments are revocation certificate's and have the
  ; same scurl and signature, false otherwise.
  ;
  ; any any -> boolean?
  (define (revo-cert=? revo1 revo2)
    (and
     (revo-cert? revo1)
     (revo-cert? revo2)
     (scurl=? (revo-cert-scurl revo1)
              (revo-cert-scurl revo2))
     (bytes=? (revo-cert-signature revo1)
              (revo-cert-signature revo2))))

  ; Creates a revocation certificate from the given scurl.
  ;
  ; private-scurl? -> revo-cert?
  (define (scurl->revo-cert scurl)
    (let ((prefix (string->bytes/utf-8 +revocation-signature-prefix+))
          (sig-base (make-host-id-base (scurl-url scurl) (scurl-key scurl))))
      (revo-cert
       scurl
       (sign (scurl-key scurl)
             (revocation-signature-digest)
             (bytes-append prefix sig-base)))))
  
  ; This function uses the given key-revocation-response-msg to validate the authenticity of
  ; the message itself.
  ;
  ; revo-cert? -> boolean?
  (define (valid-revo-cert? certificate)
    ; Validate that the host-id matches the signature.  This proves that the peer
    ; owns the private-key to the given public-key.
    (let* ((scurl (revo-cert-scurl certificate))
           (sig (revo-cert-signature certificate))
           
           ; Create the signature based used to verify the signature
           (prefix (string->bytes/utf-8 +revocation-signature-prefix+))
           (sig-base (make-host-id-base (scurl-url scurl) (scurl-key scurl))))
      ; Verify that the signature is correct against the hostID.
      (verify (scurl-key scurl)
              (revocation-signature-digest)
              sig
              (bytes-append prefix sig-base))))

