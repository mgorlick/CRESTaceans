#lang racket
(require "gen-base.rkt")
(provide (all-defined-out))

(define (start port)
  ((make-starter (string-append "audiotestsrc wave=3 ! "
                                "audioconvert ! audio/x-raw-float,channels=2,rate=44100,width=32 ! "
                                "udpsink name=udpsink host=127.0.0.1 port=" (number->string port)))))

(define pcmpl (start 4999))
pcmpl
(sleep 3)
(p/s pcmpl 4998)

(define (pcm/r) (restart pcmpl))