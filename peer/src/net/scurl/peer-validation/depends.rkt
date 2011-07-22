#lang racket/base

(require racket/contract racket/list)
  (require net/url
           (planet vyzo/crypto:2:3)
           "../common/logging-utils.rkt")
  
  (engine-load-builtin)
  
  ; Parent logger for the peer-validation module.
  (define peer-validation-parent-logger
    (make-logger 'peer-validation-parent-logger (current-logger)))
  
  (provide peer-validation-parent-logger
           (all-from-out
            net/url
            (planet vyzo/crypto:2:3)
            "../common/logging-utils.rkt"))

