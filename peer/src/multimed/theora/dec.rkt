#! /usr/bin/env racket
#lang racket

(require "../udtsrc.rkt"
         "../util.rkt"
         "theoradec.rkt")

(define (udp-in>>theora-decoder port)
  (define pid (current-thread))
  (make-pipeline (["theoradec"  : t2 (make-theora-decoder pid)]
                  ["udt-reader" : t1 (make-udt-reader pid #f port t2)])))

(define pipeline (udp-in>>theora-decoder 5000))

(semaphore-wait (make-semaphore))