#lang typed/racket

(require "event-types.rkt")
(require/typed racket/udp
               [opaque UDP udp?]
               [udp-open-socket (case-lambda
                                  [(Option String) (Option String) -> UDP]
                                  [-> UDP])]
               [udp-bind! (UDP (Option String) Natural -> Void)]
               [udp-connect! (UDP (Option String) (Option Natural) -> Void)]
               [udp-send (case-lambda 
                           [UDP Bytes -> Void]
                           [UDP Bytes Natural Natural -> Void])]
               [udp-send-to (case-lambda
                             [UDP String Natural Bytes -> Void]
                             [UDP String Natural Bytes Natural Natural -> Void])]
               [udp-close (UDP -> Void)]
               [udp-receive!-evt (UDP Bytes -> Event)])

(define-type UDP-Receive-Evt-Data (List Natural String Natural))
(define-predicate udp-receive-evt-data? UDP-Receive-Evt-Data)

(: udp-receive!-evt/t (UDP Bytes -> (Eventof UDP-Receive-Evt-Data)))
(define (udp-receive!-evt/t udp bstr)
  (Eventof (udp-receive!-evt udp bstr) udp-receive-evt-data?))

(provide (all-defined-out))