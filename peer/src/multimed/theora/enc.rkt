#! /usr/bin/env racket
#lang racket

(require "../util.rkt"
         "../udp-write.rkt"
         "theoraenc.rkt"
         "v4l2-reader.rkt")

(provide (all-defined-out))

(define pid (current-thread))

(define (make-fakesink signaller)
  (λ ()
    (define is-signaller? (make-thread-id-verifier signaller))
    (let loop ()
      (let ([r (receive-killswitch/whatever is-signaller?)])
        (cond
          [(die? r) (reply/state-report signaller #f)]
          [(bytes? r) (printf "got ~a bytes~n" (bytes-length r))
                      (loop)])))))

(define p (make-pipeline (;["udpsink"    : t3 (make-udp-writer pid "127.0.0.1" 5000)]
                          ;["fakesink"    : t3 (make-fakesink pid)]
                          ["theoraenc"   : t2 (make-theora-encoder pid pid)]
                          ["v4l2-reader" : t1 (make-v4l2-reader pid t2)])))

((make-udp-writer pid "127.0.0.1" 5000))