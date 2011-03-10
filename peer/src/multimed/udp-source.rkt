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
  
  (define buffer (make-bytes 10000))
  (define reader-sema (make-semaphore 1))
  
  (let* ([sock (udp-socket port)]
         [reader-thd (thread (λ ()
                               (let loop ()
                                 ;(semaphore-wait sema)
                                 (let*-values ([(len addr port) (udp-receive!* sock buffer)])
                                   (when len (to-all sinks <- (subbytes buffer 0 len) len)))
                                 ;(semaphore-post sema)
                                 (loop)))
                             )])
    (receive/match
     [(list (? thread? thd) 'clone-state-and-die)
      ;(semaphore-wait sema)
      (kill-thread reader-thd)
      ;(semaphore-post sema)
      (to-all parent <- 'state-report sock)
      (to-all sinks <- 'clone-state-and-die)])))