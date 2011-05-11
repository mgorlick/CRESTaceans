#! /usr/bin/env racket
#lang racket

(require "../udtsrc.rkt"
         "../util.rkt"
         "theoradec.rkt")

(provide (all-defined-out))

(define (udp-in>>theora-decoder port [theora-dec-state #f])
  (define pid (current-thread))
  (make-pipeline (["theoradec"  : t2 (if theora-dec-state 
                                         (make-theora-decoder pid theora-dec-state)
                                         (make-theora-decoder pid))]
                  ["udt-reader" : t1 (make-udt-reader pid #f port t2)])))

(define pipeline (udp-in>>theora-decoder 5000))

(define (pause/restart p)
  (command/killswitch (current-thread) (dict-ref pipeline "udt-reader"))
  (let ([states (gather-states p)])
    (udp-in>>theora-decoder 5000 (dict-ref states "theoradec"))))

(define (pr) (set! pipeline (pause/restart pipeline)))

;(semaphore-wait (make-semaphore))