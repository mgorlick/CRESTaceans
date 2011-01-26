#! /usr/bin/env racket
#lang racket

(require "../../src/net/beep-listener.rkt"
         "../../src/net/connection-manager.rkt"
         "../../src/clan.rkt"
         "../../src/net/url.rkt")

(define (showtime s)
  (printf "~a: ~a~n" (current-process-milliseconds) s))

(define (active? key)
  (manager-has-clan? bob key))

(define b->s bytes->string/utf-8)

(showtime "Start VM")
(define bob (make-manager))
(define clan1 (make-new-clan))
(manager-register-clan bob clan1)
(printf "Clan 1 PK: ~s~n" (clan-pk-urlencoded clan1))
(listen "0.0.0.0" "44037" active?)