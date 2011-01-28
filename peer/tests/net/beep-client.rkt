#! /usr/bin/env racket
#lang racket

(require "../../src/net/beep-client.rkt"
         "../../src/clan.rkt")

(define (showtime s)
  (printf "~a: ~a~n" (current-process-milliseconds) s))

(define rpk      #"L7V6aJoa3QOQg9_OXz9qc7_PrHzeHKk141dJwoGecS-H08X_hErX72NWUMGbTfBtg9w8hu8q_2A62y_gSsDJQWlyIk-YxR7K40UtIjed_Ai_tR_Va9s6YSazFU1PHGN2dzGkZTdngYMkmL0d7pN7s94yeu7eVG6RRiO7W_KGSzE"
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