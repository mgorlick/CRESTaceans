#! /usr/bin/env racket
#lang racket

(require "../../src/net/beep-client.rkt"
         "../../src/net/connection-manager.rkt"
         "../../src/clan.rkt")

(define (showtime s)
  (printf "~a: ~a~n" (current-process-milliseconds) s))

(define rpk     #"N3S5j8MF2_uACRhtC3OBUrVxcM7fdHPGPvgWJJCo2aEOwwik6QGrXtGp9qR9MG7o49kbP2mEGHo5uBbDgv7fTYNoKJWbZeRHxxAvJo6l_nGnqiTPztxMjaBcAJdFF285KRpBrDzSD9x6d6j9VPw6_fEbOZ06J17kwhz7qKgWCyc")

(define uri (string-append "crest://localhost:44037/" (bytes->string/utf-8 rpk) "/14123455"))

(showtime "Start VM")
(define my-clan (make-new-clan))
(define my-client (make-beepcli
                   ; encrypter
                   (curry clan-encrypt my-clan)
                   ; decrypter
                   (curry clan-decrypt my-clan)
                   ; calculator
                   (curry clan-mac-calc my-clan)
                   ; validator
                   (curry clan-mac-valid? my-clan)
                   ))
(beep/connect my-client uri my-clan #f #f)
(beep/start-channel my-client uri my-clan)
(beep/msg my-client uri my-clan "There is a cat in the box")

(let ([sema (make-semaphore 0)])
  (semaphore-wait sema))