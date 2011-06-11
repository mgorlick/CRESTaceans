#! /usr/bin/env racket
#lang racket/base

(require "../udp-read.rkt"
         "../util.rkt"
         "vp8dec.rkt")

(provide (all-defined-out))

(define (udp-in>>theora-decoder port [theora-dec-state #f])
  (define pid (current-thread))
  (make-pipeline (["vp8dec"     : t2 (make-vp8-decoder pid)]
                  ["udp-reader" : t1 (make-udp-reader pid #f port t2)])))

(define pipeline (udp-in>>theora-decoder 7500))

(semaphore-wait (make-semaphore))
