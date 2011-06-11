#! /usr/bin/env racket
#lang racket

(require "../tcpsrc.rkt"
         "../util.rkt"
         "vp8dec.rkt")

(provide (all-defined-out))

(define (tcp-in>>theora-decoder port [theora-dec-state #f])
  (define pid (current-thread))
  (make-pipeline (["vp8dec"     : t2 (make-vp8-decoder pid)]
                  ["tcp-reader" : t1 (make-tcp-src pid #f port t2)])))

(define pipeline (tcp-in>>theora-decoder 7500))

(semaphore-wait (make-semaphore))
