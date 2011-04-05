#lang racket

(require "../util.rkt"
         "../udp-read.rkt"
         "../udp-write.rkt"
         "../../../../bindings/theora/theora.rkt"
         "../../../../bindings/vorbis/libvorbis.rkt")

(define pid (current-thread))

(define p (make-pipeline (["udp-writer" : t2 (make-udp-writer pid "127.0.0.1" 5000)]
                          ["udp-reader" : t1 (make-udp-reader pid #f 4999 pid)])))

(define w (dict-ref p "udp-writer"))

(define/contract (a-packet p)
  (ogg-packet-pointer? . -> . boolean?)
  (printf "packet of size ~a~n" (ogg-packet-size p))
  (thread-send w (ogg-packet-data p))
  #t)

(define e (theoraenc-new))
(theoraenc-init e)
(theoraenc-foreach-header e a-packet)
(let loop ()
  (let ([bytes (thread-receive)])
    (printf "new data packet in (size ~a)~n" (bytes-length bytes))
    (theoraenc-data-in e bytes (bytes-length bytes) a-packet))
  (loop))