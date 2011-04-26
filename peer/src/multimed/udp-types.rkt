#lang typed/racket

(require "event-types.rkt")
(require/typed racket/udp
               [opaque UDP udp?]
               [udp-open-socket ((Option String) (Option String) -> UDP)]
               [udp-bind! (UDP (Option String) Exact-Nonnegative-Integer -> Void)]
               [udp-connect! (UDP (Option String) (Option Exact-Nonnegative-Integer) -> Void)]
               [udp-send (UDP Bytes -> Void)]
               [udp-send-to (UDP String Exact-Nonnegative-Integer Bytes -> Void)]
               [udp-close (UDP -> Void)]
               [udp-receive!-evt (UDP Bytes -> Event)])

(define-type UDP-Receive-Evt-Data (List Exact-Nonnegative-Integer String Exact-Nonnegative-Integer))
(define-predicate udp-receive-evt-data? UDP-Receive-Evt-Data)

(: udp-receive!-evt/t (UDP Bytes -> (Eventof UDP-Receive-Evt-Data)))
(define (udp-receive!-evt/t udp bstr)
  (Eventof (udp-receive!-evt udp bstr) udp-receive-evt-data?))

(provide (all-defined-out))