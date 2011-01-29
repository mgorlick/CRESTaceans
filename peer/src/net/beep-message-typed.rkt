#lang typed/racket

(require "base64-url-typed.rkt")

(struct: beep-message
         ([origin-pk : Bytes]
          [receiver-pk : Bytes]
          [iv : Bytes]
          [mac : Bytes]
          [body : Bytes])
         #:mutable #:transparent)

(: make-beep-message (Bytes Bytes Bytes Bytes Bytes -> beep-message))
(define (make-beep-message b1 b2 b3 b4 b5)
  (beep-message b1 b2 b3 b4 b5))

(define b64-ele #"[-|0-9|A-Z|a-z|_]+")
(define terminator-ele #"\r\n")
(define OPK-ele #"OriginPK:")
(define RPK-ele #"ReceiverPK:")
(define IV-ele  #"IV:")
(define MAC-ele #"MAC:")
(define Body-ele #"Body:")

(define b64-rx (byte-regexp b64-ele))
(define OPK-rx (byte-regexp (bytes-append OPK-ele b64-ele terminator-ele)))
(define RPK-rx (byte-regexp (bytes-append RPK-ele b64-ele terminator-ele)))
(define IV-rx (byte-regexp (bytes-append IV-ele b64-ele terminator-ele)))
(define MAC-rx (byte-regexp (bytes-append MAC-ele b64-ele terminator-ele)))
(define Body-rx (byte-regexp (bytes-append Body-ele b64-ele terminator-ele)))

(define-predicate listof-bytes? (Listof Bytes))

(: extract-field-value ([Listof Bytes] Bytes Bytes -> Bytes))
(define (extract-field-value prior-result-list ele ter-ele)
  (let ([result (car prior-result-list)])
  (subbytes result (bytes-length ele) (- (bytes-length result) (bytes-length ter-ele)))))

(: payload->beep-message (Bytes -> beep-message))
(define (payload->beep-message payload-bytes)
  (let ([ivs (regexp-match IV-rx payload-bytes)]
        [opks (regexp-match OPK-rx payload-bytes)]
        [rpks (regexp-match RPK-rx payload-bytes)]
        [macs (regexp-match MAC-rx payload-bytes)]
        [bodys (regexp-match Body-rx payload-bytes)])
    (cond
      [(and (listof-bytes? ivs)
            (listof-bytes? bodys)
            (listof-bytes? opks)
            (listof-bytes? rpks)
            (listof-bytes? macs))
       (let ([iv (extract-field-value ivs IV-ele terminator-ele)]
             [opk (extract-field-value opks OPK-ele terminator-ele)]
             [rpk (extract-field-value rpks RPK-ele terminator-ele)]
             [body (extract-field-value bodys Body-ele terminator-ele)]
             [mac (extract-field-value macs MAC-ele terminator-ele)])
         (beep-message opk rpk iv mac body))]
      [else (error "payload parse error" payload-bytes)])))

(: beep-message->payload (beep-message -> Bytes))
(define (beep-message->payload message)
  (bytes-append #"OriginPK:" (beep-message-origin-pk message) #"\r\n"
                #"ReceiverPK:" (beep-message-receiver-pk message) #"\r\n"
                #"IV:" (beep-message-iv message) #"\r\n"
                #"MAC:" (beep-message-mac message) #"\r\n"
                #"Body:" (beep-message-body message) #"\r\n"))

(define-type Mac-Validator (Bytes Bytes Bytes -> Boolean))
(define-type Decrypter (Bytes Bytes Bytes -> Bytes))

; message-validate/decrypt/decode!?:
;     (bytes bytes bytes -> boolean)
;     (bytes bytes bytes -> bytes)
; given mac validation and message decryption functions,
; replace an encrypted message with an unencrypted message
; but only if the mac validation function returns #t
(: message-validate/decrypt/decode!? (Mac-Validator
                                      Decrypter
                                      beep-message
                                      -> Boolean))
(define (message-validate/decrypt/decode!? mac-validation-function decryption-function m)
  (if (message-mac-validate mac-validation-function m)
      (message-decrypt/decode! decryption-function m)
      #f))

; message-mac-validate: given a mac validation function f:
; (f body public-key mac) -> boolean
; test whether a message m is valid according to f
;
; all relevant members of m are base64-url-encoded...for now
(: message-mac-validate (Mac-Validator beep-message -> Boolean))
(define (message-mac-validate f m)
  (f (beep-message-body m) 
     (base64-url-decode (beep-message-origin-pk m)) 
     (base64-url-decode (beep-message-mac m))))

(: message-decrypt/decode! (Decrypter beep-message -> Boolean))
; message-decrypt/decode: given a decryption function f:
; (f body iv public-key) -> decrypted-body
(define (message-decrypt/decode! f m)
  (let ([msg-bytes-decoded (base64-url-decode (beep-message-body m))]
        [remote-pk-decoded (base64-url-decode (beep-message-origin-pk m))]
        [iv-decoded (base64-url-decode (beep-message-iv m))])
    (let ([body-decoded (f msg-bytes-decoded remote-pk-decoded iv-decoded)])
      (if body-decoded
          (begin
            (set-beep-message-body! m body-decoded)
            #t)
          #f))))

(define-type Encrypter (Bytes Bytes -> (values Bytes Bytes)))
(define-type Mac-Calculator (Bytes Bytes -> Bytes))

; message-encrypt/encode: (bytes bytes -> (values bytes bytes)) bytes bytes -> (values bytes bytes)
; given a function f that encrypts an unencoded bytestring b based on the public key of the recipient,
; call f and base64-url-encode the results
(: message-encrypt/encode (Encrypter Bytes Bytes -> (values Bytes Bytes)))
(define (message-encrypt/encode f msg-bytes remote-public-key-bytes)
  (let-values ([(message-bytes-encrypted iv) (f msg-bytes remote-public-key-bytes)])
    (let ([iv-encoded (base64-url-encode iv)]
          [message-bytes-encoded (base64-url-encode message-bytes-encrypted)])
      (values message-bytes-encoded iv-encoded))))

(: mac-calculate/encode (Mac-Calculator Bytes Bytes -> (U Bytes False)))
; msg-bytes can be base64-url-encoded or not, but the choice needs to be
; consistent end to end. remote-public-key-bytes MUST NOT be url-encoded
(define (mac-calculate/encode f msg-bytes remote-public-key-bytes)
  (let ([mac (f msg-bytes remote-public-key-bytes)])
    (if mac
        (base64-url-encode mac)
        #f)))

(provide (all-defined-out))