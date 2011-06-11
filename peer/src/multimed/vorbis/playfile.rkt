#! /usr/bin/env racket
#lang racket/base

(require "../util.rkt"
         "oggdemuxer.rkt"
         "vorbisdec.rkt")

(provide (all-defined-out)
         (all-from-out "../util.rkt"))

(define (playfile)
  (define pid (current-thread))
  (make-pipeline (["vorbis-decoder" : t2 (make-vorbis-decoder pid)]
                  ["ogg-demuxer"    : t1 (make-ogg-demuxer pid "sample.ogg" t2)])))

(define p (playfile))

;(semaphore-wait (make-semaphore))