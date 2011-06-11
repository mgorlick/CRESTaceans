#lang racket/base

(require "../util.rkt"
         "../udp-read.rkt"
         "vorbisdec.rkt"
         racket/dict)

(provide (all-defined-out))

(define (udp-in>>decoder in-port [initial-vorbis-state #f])
  (define pid (current-thread))
  (define decoder (if initial-vorbis-state (make-vorbis-decoder pid initial-vorbis-state) (make-vorbis-decoder pid)))
  (make-pipeline (["vorbis-decoder" : t2 decoder]
                  ["udp-reader"     : t1 (make-udp-reader pid #f in-port t2)])))

(define (decoder:pause/move/restart pipeline new-port)
  (command/killswitch (current-thread) (dict-ref pipeline "udp-reader"))
  (let ([states (gather-states pipeline)])
    (udp-in>>decoder new-port (dict-ref states "vorbis-decoder"))))

(define decode-pipeline (udp-in>>decoder 5000))
(define (d/pmr) (set! decode-pipeline (decoder:pause/move/restart decode-pipeline 5001)))
