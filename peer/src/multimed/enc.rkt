#! /usr/bin/env racket
#lang racket

(require "udp-write.rkt"
         "udp-read.rkt"
         "vorbisenc.rkt"
         "util.rkt")
(provide (all-defined-out))

(define (udp-in>>encoder>>udp-out in-host in-port encoder-setup out-host out-port)
  (define pid (current-thread))
  (make-pipeline (["udp-writer"     : t3 (make-udp-writer pid out-host out-port)]
                  ["vorbis-encoder" : t2 (make-vorbis-encoder pid encoder-setup t3)]
                  ["udp-reader"     : t1 (make-udp-reader pid in-host in-port t2)])))

(define (encoder:pause/move/restart pipeline in-host in-port out-host out-port)
  (command/killswitch (current-thread) (dict-ref pipeline "udp-reader"))
  (let ([states (gather-states pipeline)])
    (udp-in>>encoder>>udp-out in-host in-port (dict-ref states "vorbis-encoder") out-host out-port)))

(define encode-pipeline (udp-in>>encoder>>udp-out #f 4999 (encoder-settings 1 44100 1.0 'naive) "127.0.0.1" 5000))
(define (e/pmr) (set! encode-pipeline (encoder:pause/move/restart encode-pipeline #f 4998 "127.0.0.1" 5001)))