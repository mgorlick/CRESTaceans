#lang racket

(require "base64-url.rkt")

(define-struct 
  beep-message (iv 
                origin-pk
                receiver-pk
                body)
  #:mutable
  #:transparent)

(define (beep-message->payload message)
  (bytes-append #"OriginPK:" (beep-message-origin-pk message) #"\r\n"
                #"ReceiverPK:" (beep-message-receiver-pk message) #"\r\n"
                #"IV:" (beep-message-iv message) #"\r\n"
                #"Content:" (beep-message-body message) #"\r\n"))

(define b64-ele #"[-|0-9|A-Z|a-z|_]+")
(define crlf-ele #"\r\n")
(define IV-ele #"IV:")
(define OPK-ele #"OriginPK:")
(define RPK-ele #"ReceiverPK:")
(define Content-ele #"Content:")

(define b64-rx (byte-regexp b64-ele))
(define IV-rx (byte-regexp (bytes-append IV-ele b64-ele crlf-ele)))
(define OPK-rx (byte-regexp (bytes-append OPK-ele b64-ele crlf-ele)))
(define RPK-rx (byte-regexp (bytes-append RPK-ele b64-ele crlf-ele)))
(define Content-rx (byte-regexp (bytes-append Content-ele b64-ele crlf-ele)))

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
        [cts (regexp-match Content-rx payload-bytes)])
    (cond
      [(or (empty? ivs)
           (empty? cts)
           (empty? opks)
           (empty? rpks))
       (error "payload parse error" payload-bytes)]
      [else 
       (let ([iv (extract-field-value ivs)]
             [opk (extract-field-value opks)]
             [rpk (extract-field-value rpks)]
             [ct (extract-field-value cts)])
         (beep-message iv opk rpk ct))])))

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

(define (message-encrypt/encode f msg-bytes remote-public-key-bytes)
  (let-values ([(message-bytes-encrypted iv) (f msg-bytes remote-public-key-bytes)])
    (let ([iv-encoded (base64-url-encode iv)]
          [message-bytes-encoded (base64-url-encode message-bytes-encrypted)])
      (values message-bytes-encoded iv-encoded))))

(provide beep-message 
         beep-message? 
         beep-message-iv
         beep-message-body
         beep-message-origin-pk
         beep-message-receiver-pk)
(provide/contract
 [beep-message->payload (beep-message? . -> . bytes?)]
 [payload->beep-message (bytes? . -> . beep-message?)]
 [message-encrypt/encode ((bytes? bytes? . -> . (values (or/c #f bytes?) (or/c #f bytes?)))
                          bytes? bytes?
                          . -> . (values (or/c #f bytes?) (or/c #f bytes?)))]
 [message-decrypt/decode! ((bytes? bytes? bytes? . -> . (or/c #f bytes?))
                           beep-message? . -> . boolean?)])