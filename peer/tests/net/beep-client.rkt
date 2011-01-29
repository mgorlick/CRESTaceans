#! /usr/bin/env racket
#lang racket

(require "../../src/net/beep-client.rkt"
         "../../src/clan.rkt")

(define (showtime s)
  (printf "~a: ~a~n" (current-process-milliseconds) s))

(define rpk       #"cQhBb4AQDfBT-w5cGp5hN4L6h1IVnxtV_teHFuwUO0kSmmDThNKhzJkmOuhDbpBRbYQq0LvHemaQH-YfQkhoEDSBVm_7A9rgBrT_RJF_sb485473ypWD1_PKr9x9PDY72sLhuJ0-1bxHejr3AAwdKKnS7zPcHYrYHo4jREFbjY8"
  )

(define uri (string-append "crest://localhost:44037/" (bytes->string/utf-8 rpk) "/14123455"))
(showtime "Start VM")
(define my-clan (make-new-clan))
(showtime "Clan made")
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
(showtime "Client made")
(beep/connect my-client uri my-clan #f #f)
(showtime "Connected")
(define sema (make-semaphore 0))
(beep/start-channel my-client uri my-clan
                    (λ (connection channel frame message)
                      (showtime "Echo returned")
                      ;(semaphore-post sema)
                      ))
(showtime "Channel started")
(beep/msg my-client uri my-clan "There is a cat in the box")
(showtime "Message sent")
(beep/msg my-client uri my-clan "There is a cat in the box")
(showtime "Message sent")
(beep/msg my-client uri my-clan "There is a cat in the box")
(showtime "Message sent")
(beep/msg my-client uri my-clan "There is a cat in the box")
(showtime "Message sent")
(beep/msg my-client uri my-clan "There is a cat in the box")
(showtime "Message sent")
(beep/msg my-client uri my-clan "There is a cat in the box")
(showtime "Message sent")
(beep/msg my-client uri my-clan "There is a cat in the box")
(showtime "Message sent")
(semaphore-wait sema)