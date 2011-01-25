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

(go #"F7fMEiCi1-3vuGiUXL30fSH50pFsz-ykYkRQo9_-iZHTKzSC_YtiaKhtbRuGtdXHEE1oHEWolZ5eXoRDXZ7AWpVsfqvD05fRN5bIifPZ5di_1t1B64tYmI-3Iwr8DJVaDMMxIL9eyFzsj1IigC9qz2spqVXfxCX3eSyg1nUJNM0"
)
(showtime "Connected")