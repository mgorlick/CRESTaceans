#! /usr/bin/env racket
#lang racket

(require "../../src/net/beep-client.rkt"
         "../../src/net/url.rkt"
         "../../src/net/connection-manager.rkt"
         "../../src/clan.rkt")

(define (showtime s)
  (printf "~a: ~a~n" (current-process-milliseconds) s))

(define (connect rpk)
  (let ([uri (string-append "crest://localhost:44037/" (bytes->string/utf-8 rpk) "/14123455")])
    (printf "Connecting to clan @ uri ~a ~n" uri)
    (beepcli-connect clan2client uri clan2 #f #f)))

(define (disconnect rpk)
  (let ([uri (string-append "crest://localhost:44037/" (bytes->string/utf-8 rpk) "/14123455")])
    (printf "Disconnecting from clan @ uri ~a ~n" uri)
    (beepcli-disconnect clan2client uri clan2)))

(define rpk   #"VqZ2pGeapvx7O5v75KfdhQlpiXKedGIhBv7pivfVp9Y9-cNuoDO-WsDHm3hTUljb-GYIsRvouLRdSMxwdnBrQQDEoHtRms3DGTYnwz4sNdsuj5xgTk3c6z49X3Y38tyFUaQmzLOmJHGl1PdFkerDQ7YedB7e12A4a170wm8bQa8"
)

(showtime "Start VM")
(define alice (make-manager))
(showtime "Made manager")
(define clan2 (make-new-clan))
(showtime "Made clan")
(manager-register-clan alice clan2)
(showtime "Registered clan")
(define clan2client (make-beepcli alice))
(showtime "Made client")
(connect rpk)
(showtime "Connected (or not)")
(disconnect rpk)
(showtime "Disconnected")