#lang racket

(require "../../src/splunk/splunk-client.rkt")

(define c (make-splunk-client "http://localhost:8089" "admin" "morefuntocompute"))
(define-values (avahi-code avahi-results) (splunk-search c "search avahi"))
(length avahi-results)