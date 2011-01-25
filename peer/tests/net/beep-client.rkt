#! /usr/bin/env racket
#lang racket

(require "../../src/net/beep-client.rkt"
         "../../src/net/url.rkt"
         "../../src/net/connection-manager.rkt"
         "../../src/clan.rkt")

(define (showtime s)
  (printf "~a: ~a~n" (current-process-milliseconds) s))

(showtime "Start VM")
(define alice (make-manager))
(showtime "Made manager")
(define clan2 (make-new-clan))
(showtime "Made clan")
(manager-register-clan alice clan2)
(showtime "Registered clan")
(define clan2client (make-beepcli alice))
(showtime "Made client")
(define (go rpk)
  (let ([uri (string-append "crest://localhost:44000/" (bytes->string/utf-8 rpk) "/14123455/path;param?hello=world")])
    (printf "Connecting to clan @ uri ~a ~n" uri)
    (beepcli-connect clan2client uri
                     clan2 #f #f)))

(go  #"MGTL3I_MqZaReo0cg0foQAYw2mlaYEixz3jluOcEwNz0q3xO70V1uexzq8ctrC-8pIN2AbS8zH4W4Xz7hhbcot1ef_fy_fgsdD1WD7itmWrKbxZjoS5FpUitFbfM1CV_U-GyDOlDCLinGb7ms5a9xhax5WKFUwoDWl2PnNTFHXw"
)
(showtime "Connected")