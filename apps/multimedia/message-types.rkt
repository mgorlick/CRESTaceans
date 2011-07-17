#lang racket/base

(require "pipeline/structs.rkt")
(provide (all-defined-out)
         (all-from-out "pipeline/structs.rkt"))

(struct Quit () #:transparent)
(struct None () #:transparent)