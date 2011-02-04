#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         "../../gst/gstreamer.rkt")
(provide (except-out (all-defined-out) test4-wrap df-t4))

(define test4-wrap (ffi-lib "libracket-gst-test4" "1.0"))
(define-ffi-definer df-t4 test4-wrap)

(df-t4 t4_signal_connect (_fun _GstElement-pointer ((_or-null _GstElement-pointer) = #f) -> _gulong))
