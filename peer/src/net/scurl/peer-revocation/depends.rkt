#lang racket
  (require net/url
           (planet vyzo/crypto:2:3)
           "../common/logging-utils.rkt")
  
  ; Parent logger for the revocation module.
  (define revocation-parent-logger
    (make-logger 'revocation-parent-logger (current-logger)))
  
  (provide revocation-parent-logger
           (all-from-out
            net/url
            (planet vyzo/crypto:2:3)
            "../common/logging-utils.rkt"))

