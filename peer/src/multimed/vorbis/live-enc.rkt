#! /usr/bin/env racket
#lang racket

(require "../util.rkt"
         "../pulsesrc.rkt"
         "../udtsink.rkt"
         "vorbisenc.rkt"
         "vorbisdec.rkt")

(provide (all-defined-out)
         (all-from-out "../util.rkt"))

(define pid (current-thread))

(define (makep) (make-pipeline (["udt" : t3 (make-udt-writer pid "127.0.0.1" 5000)]
                                ["enc" : t2 (make-vorbis-encoder pid (encoder-settings 2 44100 1.0 'naive) t3)]
                                ["mic" : t1 (make-pulsesrc pid t2)])))

(define p (makep))

(semaphore-wait (make-semaphore))