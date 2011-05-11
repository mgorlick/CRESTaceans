#! /usr/bin/env racket
#lang racket

(require "../util.rkt"
         "../udtsink.rkt"
         "../fork.rkt"
         "vp8enc.rkt"
         "vp8dec.rkt"
         "v4l2-reader.rkt")

(provide (all-defined-out)
         (all-from-out "../fork.rkt"))

(define pid (current-thread))

(define p (make-pipeline (["vp8dec"      : t4 (make-vp8-decoder pid)]
                          ["fork"        : t3 (make-fork pid (list t4))]
                          ["vp8"         : t2 (make-vp8-encoder pid t3)]
                          ["v4l2-reader" : t1 (make-v4l2-reader pid t2)])))

(define t5 (thread (make-udt-writer pid "127.0.0.1" 5000)))

(define fork (dict-ref p "fork"))