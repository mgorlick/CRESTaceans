#lang racket

(require ffi/unsafe
         scribble/srcdoc
         "glib-ffi.rkt")

(require/doc racket/base
            scribble/manual)

(provide (all-defined-out) 
         (all-from-out ffi/unsafe) 
         (all-from-out scribble/srcdoc)
         (all-from-out "glib-ffi.rkt"))


; FFI
(define gstreamer-lib (ffi-lib "libgstreamer-0.10"))




;(define-syntax-rule pattern template) -- The simplest way to create a macro
(define-syntax-rule (define-gstreamer obj typ)
  (define obj (get-ffi-obj 'obj gstreamer-lib typ)))

(define-syntax-rule (define-gstreamer* typ obj ...)
  (begin (define-gstreamer obj typ)
         ...))

