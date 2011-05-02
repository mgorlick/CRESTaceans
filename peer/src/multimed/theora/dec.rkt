#! /usr/bin/env racket
#lang racket

(require "../udtsrc.rkt"
         "../../../../bindings/theora/theora.rkt"
         "../util.rkt")

(define (udp-in>>theora-decoder port)
  (define pid (current-thread))
  (make-pipeline (["udt-reader" : t1 (make-udt-reader pid #f port pid)])))

(define pipeline (udp-in>>theora-decoder 5000))

(define d (theoradec-new))
(printf "(it's safe to run encoder now)~n")

(let loop ()
  (let ([bytes (thread-receive)])
    (cond [(not (theoradec-ready-for-data d)) (theoradec-header-in d bytes)]
          [else (theoradec-data-in d bytes)])
    (loop)))

(theoradec-delete d)