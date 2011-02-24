#lang racket

(require "util.rkt"
         (planet bzlib/thread:1:0))

(provide udp-source)

;; UDP source component

(define (udp-source port parent . sinks)
  (define (udp-socket port)
    (let ([s (udp-open-socket)])
      (udp-bind! s #f port)
      s))
  
  (let ([sock (udp-socket port)]
        [buffer (make-bytes 10000)])
    (let loop ()
      (let*-values 
          ([(len addr port) (udp-receive! sock buffer)]
           [(subbuffer) (bytes->immutable-bytes (subbytes buffer 0 len))])
        (to-all sinks <- subbuffer)
        (loop)))))