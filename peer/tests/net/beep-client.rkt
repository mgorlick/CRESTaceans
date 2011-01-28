#! /usr/bin/env racket
#lang racket

(require "../../src/net/beep-client.rkt"
         "../../src/net/connection-manager.rkt"
         "../../src/clan.rkt")

(define (showtime s)
  (printf "~a: ~a~n" (current-process-milliseconds) s))

(define rpk    #"lMfmFXg9yI5O7UFdmWWX9CeIX8E_OLJVVGcuKpiELPpaQBFDITyCbRKKDydPuVXIvmkyNl5BJC3_Vq0f_I5oxxtQ_wwpZUwKUVIlaxtimWEb3YOU9Qpcvp8uFSjOGT7OznbWZOOzaCBdsbQZ3H_aPhuhkz4Q7eBomKNBbQaCtGc"

)

(define uri (string-append "crest://localhost:44037/" (bytes->string/utf-8 rpk) "/14123455"))

(showtime "Start VM")
(define my-clan (make-new-clan))
(define my-client (make-beepcli
                   ; encrypter
                   (curry clan-encrypt my-clan)
                   ; decrypter
                   (curry clan-decrypt my-clan)
                   ; validator
                   (curry clan-validate my-clan)
                   ))
(beep/connect my-client uri my-clan #f #f)
(beep/start-channel my-client uri my-clan)
(beep/msg my-client uri my-clan "There is a cat in the box")


(let ([sema (make-semaphore 0)])
  (semaphore-wait sema))