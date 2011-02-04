#lang racket

(require "../../src/splunk/splunk-client.rkt")

(define c (make-splunk-client "http://localhost:8089" "admin" "morefuntocompute"))
(define sid (start-search c "search avahi"))
sid
(sleep 1)
(let loop ()
  (if (check-on-search c sid)
      (get-search-results c sid)
      (begin (sleep 1) (loop))))