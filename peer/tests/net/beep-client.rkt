#! /usr/bin/env racket
#lang racket

(require "../../src/net/beep-client.rkt"
         "../../src/clan.rkt")

(define (showtime s)
  (printf "~a: ~a~n" (current-milliseconds) s))

(define rpk       #"Wn5g9hv8xGHeJjNAVcpcRG5t9H35awBU0FFqmTqK4JTVHoWCGhD1bTZQ_SWU5zTe3nxneDwfjQRgxxETSIo3xJeKatnT33eZjddRrX402ZRUhozbHXAE5gsVx9_1fnRsLxCt3KUlou6sojtsvKZlyWCrduPUenXUGyI-fuBc5GY"

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