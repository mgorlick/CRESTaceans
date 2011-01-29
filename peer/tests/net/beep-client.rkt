#! /usr/bin/env racket
#lang racket

(require "../../src/net/beep-client.rkt"
         "../../src/clan.rkt")

(define (showtime s)
  (printf "~a: ~a~n" (current-process-milliseconds) s))

(define rpk       #"bxKU-_N_qNLTaBhM1HIO0qlC80ISYOaNc0A26oyBIyOrpt23jG5YNYoEnKPMsf3FIAK1ws4UrEolVA_5U5IzIY1XMeS_cMNmhohqc2dmD4NWolNxpo4-fDOZJlh6rCVvt6_CFJnmFm6oDC-GJ30_9fWHF2jJl4IYz4kaYxaqxtY"

  )

(define uri (string-append "crest://localhost:44037/" (bytes->string/utf-8 rpk) "/14123455"))
(showtime "Start VM")
(define my-clan (make-new-clan))
(showtime "Clan made")
(define my-client (make-beepcli
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