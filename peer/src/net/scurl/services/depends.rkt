#lang racket/base

(require racket/contract racket/list)
  (require net/url
           (planet vyzo/crypto:2:3)
           "../common/logging-utils.rkt")
  
  ; Parent logger for the services module.
  (define services-parent-logger
    (make-logger 'services-parent-logger (current-logger)))
  
  (provide services-parent-logger
           (all-from-out
            net/url
            (planet vyzo/crypto:2:3)
            "../common/logging-utils.rkt"))

