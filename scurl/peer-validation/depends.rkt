#lang racket
  (require net/url
           (planet vyzo/crypto:2:3)
           "../common/logging-utils.rkt")
  
  ; Parent logger for the peer-validation module.
  (define peer-validation-parent-logger
    (make-logger 'peer-validation-parent-logger (current-logger)))
  
  (provide peer-validation-parent-logger
           (all-from-out
            net/url
            (planet vyzo/crypto:2:3)
            "../common/logging-utils.rkt"))

