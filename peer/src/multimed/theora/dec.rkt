#lang racket

(require "../udp-read.rkt"
         "../util.rkt")

(define (udp-in>>theora-decoder port)
  (define pid (current-thread))
  (make-pipeline (["udp-reader" : t1 (make-udp-reader pid #f port pid)])))

(define pipeline (udp-in>>theora-decoder 5000))

(let loop ()
  (let ([bytes (thread-receive)])
    (printf "Bytes in: ~a~n" (bytes-length bytes))
    (loop)))