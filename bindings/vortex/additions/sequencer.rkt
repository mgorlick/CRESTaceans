#lang racket

(require (except-in ffi/unsafe ->)
         "../vtx/module.rkt")
(provide (all-defined-out))

;; sequencer replacement