#! /usr/bin/env racket
#lang racket

(require "../udtsrc.rkt"
         "../util.rkt"
         "vp8dec.rkt")

(provide (all-defined-out))

(define (udp-in>>theora-decoder port [theora-dec-state #f])
  (define pid (current-thread))
  (make-pipeline (["vp8dec"     : t2 (make-vp8-decoder pid)]
                  ["udt-reader" : t1 (make-udt-reader pid #f port t2)])))

(define pipeline (udp-in>>theora-decoder 5000))

(semaphore-wait (make-semaphore))