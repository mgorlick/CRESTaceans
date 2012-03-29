#! /usr/bin/env racket
#lang racket/base

(require "../../compile/compile.rkt"
         "../../generate/baseline.rkt"
         "../../baseline.rkt")

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define motile-compiled-source
  (motile/compile
   '(let ()
      (define (tak x y z)
        (if (not (< y x))
            z
            (tak (tak (- x 1) y z)
                 (tak (- y 1) z x)
                 (tak (- z 1) x y))))
      (tak 18 12 6))))

(define (racket-version)
  (tak 18 12 6))
(define (motile-version)
  (motile/call motile-compiled-source BASELINE))

(define (iterations n f)
  (for ([i (in-range n)])
    (f)))

(collect-garbage)(collect-garbage)(collect-garbage)(collect-garbage)
(time (iterations 100 racket-version))
(collect-garbage)(collect-garbage)(collect-garbage)(collect-garbage)
(time (iterations 100 motile-version))
(collect-garbage)(collect-garbage)(collect-garbage)(collect-garbage)