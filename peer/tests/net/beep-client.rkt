#! /usr/bin/env racket
#lang racket

(require "../../src/net/beep-client.rkt"
         "../../src/clan.rkt")

(define (showtime s)
  (printf "~a: ~a~n" (current-process-milliseconds) s))

(define rpk     #"Y4ayloDwDlRkHgtjDKH3RF0_bZ7NPdH66rktV4sOzJpvLrhxi0jd4b-NJC7obcJMyeaOQFDOwVG0mJAluFiAbECpTL39MdJ4biCdaqkUzyJrs010Hn8qaWSsySs6EleZF6-nXBvPq_6bMhXCf_5WXafxEYnSJ6wz1KWvYvRYo_s")

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
(define sema (make-semaphore 0))
(beep/start-channel my-client uri my-clan
                    (λ (connection channel frame message)
                      (semaphore-post sema)))
(beep/msg my-client uri my-clan "There is a cat in the box")
(semaphore-wait sema)