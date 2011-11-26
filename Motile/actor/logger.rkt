#lang racket/base


; For preliminary testing and debugging.
(define DEBUG-RECEIVER (make-log-receiver (current-logger) 'info))
(define BLABBER
  (thread
   (lambda()
     (let loop ((x (sync DEBUG-RECEIVER)))
       (display (format "~a: ~a\n" (vector-ref x 0) (vector-ref x 1)) (current-error-port))
       (loop (sync DEBUG-RECEIVER))))))
