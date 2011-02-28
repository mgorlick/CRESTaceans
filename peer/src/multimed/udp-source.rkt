#lang racket

(require "util.rkt"
         (planet bzlib/thread:1:0))

(provide udp-source)

;; UDP source component

(define (udp-source parent port . sinks)
  (define (udp-socket port)
    (let ([s (udp-open-socket)])
      (udp-bind! s #f port)
      s))
  
  (let* ([sock (udp-socket port)]
         [evt (udp-receive-ready-evt sock)]
         [buffer (make-bytes 10000)])
    (let loop ()
      (receive/match
       [(list (? thread? thd) 'clone-state-and-die)
        (to-all parent <- 'state-report sock)
        (to-all sinks <- 'clone-state-and-die)]
       
       [after 0
              (if (sync/timeout 0 evt)
                  (let*-values 
                      ([(len addr port) (udp-receive! sock buffer)]
                       [(subbuffer) (bytes->immutable-bytes (subbytes buffer 0 len))])
                    (to-all sinks <- subbuffer len)
                    (loop))
                  (loop))]))))