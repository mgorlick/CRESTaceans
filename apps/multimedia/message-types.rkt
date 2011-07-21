#lang racket/base

(require "../../Motile/struct.rkt"
         "pipeline/structs.rkt")
(provide (all-defined-out)
         (all-from-out "pipeline/structs.rkt"))

(define-motile-struct AddCURL [curl])
(define-motile-struct RemoveCURL [curl])
(define-motile-struct Quit [])
(define-motile-struct None [])
(define-motile-struct Frame [data timestamp])

#|(define b (AddCURL 'foo))
b
(AddCURL? b)
(AddCURL.curl b)
(AddCURL? (AddCURL!curl b 'bar))|#