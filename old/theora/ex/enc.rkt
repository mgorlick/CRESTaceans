#! /usr/bin/env racket
#lang racket

(require "../util.rkt"
         "../udtsink.rkt"
         "theoraenc.rkt"
         "v4l2-reader.rkt")

(provide (all-defined-out))

(define pid (current-thread))

(define p (make-pipeline (["udtsink"     : t3 (make-udt-writer pid "127.0.0.1" 5000)];"128.195.58.146" 5000)]
                          ["theoraenc"   : t2 (make-theora-encoder pid t3)]
                          ["v4l2-reader" : t1 (make-v4l2-reader pid t2)])))

(semaphore-wait (make-semaphore))