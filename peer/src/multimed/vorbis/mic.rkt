#! /usr/bin/env racket
#lang racket

(require "../util.rkt"
         "../pulsesrc.rkt"
         "vorbisenc.rkt"
         "vorbisdec.rkt")

(provide (all-defined-out)
         (all-from-out "../util.rkt"))

(define pid (current-thread))

(define (makep) (make-pipeline (["dec" : t3 (make-vorbis-decoder pid)]
                                ["enc" : t2 (make-vorbis-encoder pid (encoder-settings 2 44100 1.0 'naive) t3)]
                                ["mic" : t1 (make-pulsesrc pid t2)])))

(define p (makep))

;(semaphore-wait (make-semaphore))