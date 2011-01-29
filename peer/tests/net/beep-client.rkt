#! /usr/bin/env racket
#lang racket

(require "../../src/net/beep-client.rkt"
         "../../src/clan.rkt")

(define (showtime s)
  (printf "~a: ~a~n" (current-process-milliseconds) s))

(define rpk       #"ka2f3cmpnqVA35yE9M7zljDzTgkpk3kC6B7ealM0wQkWWbjlFdAjaQLOyUSDFxx4QtMvYf-xcDh4Xc1h9lL-OJLwTju1AUnC8EX84rXPze6Zi31_A--puA_UIFVyE3SUgmaiDSEBn8wKcDgNL-ExQHQNpSUZl5KTf-wtL3Ol0bM"
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