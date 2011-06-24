#lang racket
  (require (planet vyzo/crypto:2:3)
           "../common/logging-utils.rkt")
  
  ; Parent logger for the peer-secure-session module.
  (define peer-secure-session-parent-logger
    (make-logger 'peer-secure-session-parent-logger (current-logger)))
  
  (provide peer-secure-session-parent-logger
           (all-from-out
            (planet vyzo/crypto:2:3)
            "../common/logging-utils.rkt"))

