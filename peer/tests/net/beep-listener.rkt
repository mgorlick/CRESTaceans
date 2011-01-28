#! /usr/bin/env racket
#lang racket

(require "../../src/net/beep-listener.rkt"
         "../../src/net/connection-manager.rkt"
         "../../src/net/base64-url.rkt"
         "../../src/clan.rkt"
         )

(define b->s bytes->string/utf-8)
(define (active? key) (manager-has-clan? my-manager key))
(define my-manager (make-manager))
(define my-clan (make-new-clan))
(define encrypter (curry clan-encrypt my-clan))
(define decrypter (curry clan-decrypt my-clan))
(define validator (curry clan-mac-valid? my-clan))
(define calculator (curry clan-mac-calc my-clan))

(define (in-response connection channel frame message)
  (let ([local-public-key-encoded (beep-message-receiver-pk message)]
        [remote-public-key-encoded (beep-message-origin-pk message)])
    (let-values ([(msg-bytes-encoded iv-encoded)
                  (message-encrypt/encode encrypter
                                          (beep-message-body message)
                                          (base64-url-decode remote-public-key-encoded))])
      (let* ([mac-encoded (mac-calculate/encode 
                           calculator msg-bytes-encoded remote-public-key-encoded)]
             [reply (beep-message local-public-key-encoded
                                  remote-public-key-encoded 
                                  iv-encoded
                                  mac-encoded
                                  msg-bytes-encoded)]
             [payload (beep-message->payload reply)])
        (let-values ([(success? msgno) (vortex-channel-send-msg channel payload)])
          (void))))))

(manager-register-clan my-manager my-clan)
(printf "Clan 1 PK: ~s~n" (clan-pk-urlencoded my-clan))
(listen "0.0.0.0" "44037" active? encrypter decrypter calculator validator #f in-response)