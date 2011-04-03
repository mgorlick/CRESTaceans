#lang racket

(require "../util.rkt"
         "../udp-read.rkt"
         "../../../../bindings/theora/theora.rkt"
         "../../../../bindings/vorbis/libvorbis.rkt")

(define pid (current-thread))

(define p (make-pipeline (["udp-reader" : t1 (make-udp-reader pid #f 5000 pid)])))

(define/contract (a-packet p)
  (ogg-packet-pointer? . -> . boolean?)
  (printf "packet of size ~a~n" (ogg-packet-size p))
  #t)

(define e (theoraenc-new))
(theoraenc-init e)
(theoraenc-delete e)