#lang racket
(require "gen-base.rkt")
(provide (all-defined-out))

(define (start port)
  ((make-starter (string-append "filesrc location=orbits.wav ! wavparse ! "
                                "audioconvert ! audio/x-raw-float,channels=1,rate=44100,width=32 ! "
                                "udpsink name=udpsink host=127.0.0.1 port=" (number->string port)))))

(define pcmpl (start 4999))
;(sleep 3)
;(p/s pcmpl 4998)

(define (pcm/r) (restart pcmpl))
