#lang racket

(require "base64-url-typed.rkt")

(define-struct 
  beep-message (origin-pk
                receiver-pk
                iv
                mac
                body)
  #:mutable
  #:transparent)

(define (beep-message->payload message)
  (bytes-append #"OriginPK:" (beep-message-origin-pk message) #"\r\n"
                #"ReceiverPK:" (beep-message-receiver-pk message) #"\r\n"
                #"IV:" (beep-message-iv message) #"\r\n"
                #"MAC:" (beep-message-mac message) #"\r\n"
                #"Body:" (beep-message-body message) #"\r\n"))

(define b64-ele #"[-|0-9|A-Z|a-z|_]+")
(define crlf-ele #"\r\n")
(define OPK-ele #"OriginPK:")
(define RPK-ele #"ReceiverPK:")
(define IV-ele #"IV:")
(define MAC-ele #"MAC:")
(define Body-ele #"Body:")

(define b64-rx (byte-regexp b64-ele))
(define OPK-rx (byte-regexp (bytes-append OPK-ele b64-ele crlf-ele)))
(define RPK-rx (byte-regexp (bytes-append RPK-ele b64-ele crlf-ele)))
(define IV-rx (byte-regexp (bytes-append IV-ele b64-ele crlf-ele)))
(define MAC-rx (byte-regexp (bytes-append MAC-ele b64-ele crlf-ele)))
(define Body-rx (byte-regexp (bytes-append Body-ele b64-ele crlf-ele)))

(define (extract-field-value prior-result-list)
  (let ([value-list (regexp-match* b64-rx (first prior-result-list))])
    (cond
      [(empty? value-list) (error "payload parse error" prior-result-list)]
      [else (second value-list)])))

; payload->beep-message: bytes -> beep-message
(define (payload->beep-message payload-bytes)
  (let ([ivs (regexp-match IV-rx payload-bytes)]
        [opks (regexp-match OPK-rx payload-bytes)]
        [rpks (regexp-match RPK-rx payload-bytes)]
        [macs (regexp-match MAC-rx payload-bytes)]
        [bodys (regexp-match Body-rx payload-bytes)])
    (cond
      [(or (empty? ivs)
           (empty? bodys)
           (empty? opks)
           (empty? rpks)
           (empty? macs))
       (error "payload parse error" payload-bytes)]
      [else 
       (let ([iv (extract-field-value ivs)]
             [opk (extract-field-value opks)]
             [rpk (extract-field-value rpks)]
             [body (extract-field-value bodys)]
             [mac (extract-field-value macs)])
         (beep-message opk rpk iv mac body))])))

; message-validate/decrypt/decode!?:
;     (bytes bytes bytes -> boolean)
;     (bytes bytes bytes -> bytes)
; given mac validation and message decryption functions,
; replace an encrypted message with an unencrypted message
; but only if the mac validation function returns #t
(define (message-validate/decrypt/decode!? mac-validation-function decryption-function m)
  (if (message-mac-validate mac-validation-function m)
      (message-decrypt/decode! decryption-function m)
      #f))

; message-mac-validate: given a mac validation function f:
; (f body public-key mac) -> boolean
; test whether a message m is valid according to f
;
; all relevant members of m are base64-url-encoded...for now
(define (message-mac-validate f m)
  (f (beep-message-body m) 
     (base64-url-decode (beep-message-origin-pk m)) 
     (base64-url-decode (beep-message-mac m))))

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

; message-encrypt/encode: (bytes bytes -> (values bytes bytes)) bytes bytes -> (values bytes bytes)
; given a function f that encrypts an unencoded bytestring b based on the public key of the recipient,
; call f and base64-url-encode the results
(define (message-encrypt/encode f msg-bytes remote-public-key-bytes)
  (let-values ([(message-bytes-encrypted iv) (f msg-bytes remote-public-key-bytes)])
    (let ([iv-encoded (base64-url-encode iv)]
          [message-bytes-encoded (base64-url-encode message-bytes-encrypted)])
      (values message-bytes-encoded iv-encoded))))

; msg-bytes can be base64-url-encoded or not, but the choice needs to be
; consistent end to end. remote-public-key-bytes MUST NOT be url-encoded
(define (mac-calculate/encode f msg-bytes remote-public-key-bytes)
  (let ([mac (f msg-bytes remote-public-key-bytes)])
    (if mac
        (base64-url-encode mac)
        #f)))

(provide beep-message 
         beep-message? 
         beep-message-iv
         beep-message-mac
         beep-message-body
         beep-message-origin-pk
         beep-message-receiver-pk
         make-beep-message)
(provide/contract
 [beep-message->payload (beep-message? . -> . bytes?)]
 [payload->beep-message (bytes? . -> . beep-message?)]
 [message-encrypt/encode ((bytes? bytes? . -> . (values (or/c #f bytes?) (or/c #f bytes?)))
                          bytes? bytes?
                          . -> . (values (or/c #f bytes?) (or/c #f bytes?)))]
 [message-decrypt/decode! ((bytes? bytes? bytes? . -> . (or/c #f bytes?))
                           beep-message? . -> . boolean?)]
 [mac-calculate/encode ((bytes? bytes? . -> . bytes?)
                        bytes? bytes? . -> . (or/c #f bytes?))]
 [message-validate/decrypt/decode!? ((bytes? bytes? bytes? . -> . boolean?)
                                     (bytes? bytes? bytes? . -> . (or/c #f bytes?))
                                     beep-message? . -> . boolean?)])