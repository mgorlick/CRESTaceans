#! /usr/bin/env racket
#lang racket

(require "../udp-read.rkt"
         "../../../../bindings/theora/theora.rkt"
         "../util.rkt")

(define (udp-in>>theora-decoder port)
  (define pid (current-thread))
  (make-pipeline (["udp-reader" : t1 (make-udp-reader pid #f port pid)])))

(define pipeline (udp-in>>theora-decoder 5000))

(define d (theoradec-new))
(printf "(it's safe to run encoder now)~n")

(let loop ()
  (let ([bytes (thread-receive)])
    (printf "Bytes in: ~a~n" (bytes-length bytes))
    (printf "~a~n" (cond [(not (theoradec-ready-for-data d)) (theoradec-header-in d bytes)]
                         [else (theoradec-data-in d bytes)]))
    (loop)))

(theoradec-delete d)