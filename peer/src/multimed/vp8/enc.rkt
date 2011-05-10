#! /usr/bin/env racket
#lang racket

(require "../util.rkt"
         "../udtsink.rkt"
         "vp8enc.rkt"
         "vp8dec.rkt"
         "v4l2-reader.rkt")

(provide (all-defined-out))

(define pid (current-thread))

(define p (make-pipeline (;["udtsink"     : t3 (make-udt-writer pid "127.0.0.1" 5000)];"128.195.58.146" 5000)]
                          ["vp8dec"      : t3 (make-vp8-decoder pid)]
                          ["vp8"         : t2 (make-vp8-encoder pid t3)]
                          ["v4l2-reader" : t1 (make-v4l2-reader pid t2)])))

(semaphore-wait (make-semaphore))