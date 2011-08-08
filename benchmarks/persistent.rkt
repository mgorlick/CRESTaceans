#! /usr/bin/env racket
#lang racket/base

(require "../Motile/persistent/vector.rkt"
         "../Motile/persistent/set.rkt"
         "../Motile/persistent/hash.rkt")

"Vector"
(for ([i (in-range 1)])
  (time (for/fold ([v vector/null])
          ([i (in-range 1000000)])
          (vector/cons v i))))

"Set"
(for ([i (in-range 1)])
  (time (for/fold ([s set/equal/null])
          ([i (in-range 1000000)])
          (set/cons s i))))

"Hash"
(for ([i (in-range 1)])
  (time (for/fold ([h hash/equal/null])
          ([i (in-range 1000000)])
          (hash/cons h i i))))