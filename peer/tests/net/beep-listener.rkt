#! /usr/bin/env racket
#lang racket

(require "../../src/net/beep-listener.rkt"
         "../../src/net/connection-manager.rkt"
         "../../src/clan.rkt"
         "../../src/net/url.rkt")

(define (showtime s)
  (printf "~a: ~a~n" (current-process-milliseconds) s))

(define b->s bytes->string/utf-8)

(showtime "Start VM")
(define bob (make-manager))
(showtime "Made manager")
(define clan1 (make-new-clan))
(showtime "Made clan")
(manager-register-clan bob clan1)
(showtime "Registered clan")
(printf "Clan 1 PK: ~s~n" (base64-url-encode (clan-pk clan1)))
(listen "0.0.0.0" "44000" bob)