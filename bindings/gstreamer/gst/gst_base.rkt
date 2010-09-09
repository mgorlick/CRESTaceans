#lang racket

(require ffi/unsafe
         "glib-ffi.rkt")

(provide (all-defined-out) (all-from-out ffi/unsafe) (all-from-out "glib-ffi.rkt"))

; FFI
(define gstreamer-lib (ffi-lib "libgstreamer-0.10"))

(define _charptr (_ptr io _byte))
(define _intptr (_ptr io _int))


;(define-syntax-rule pattern template) -- The simplest way to create a macro
(define-syntax-rule (define-gstreamer obj typ)
  (define obj (get-ffi-obj 'obj gstreamer-lib typ)))

(define-syntax-rule (define-gstreamer* typ obj ...)
  (begin (define-gstreamer obj typ)
         ...))


