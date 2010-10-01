#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         "../gst/gstreamer.rkt")
(provide (except-out (all-defined-out) df))

(define-ffi-definer df (ffi-lib "libracket-gst" "1.0"))

(df signal_connect (_fun _GstElement-pointer _GstElement-pointer -> _void))

(df gst_message_type (_fun _GstMessage-pointer -> _int))
(df gst_message_unref_w (_fun _GstMessage-pointer -> _void))

(df print_gst_time_format (_fun _gint64 _gint64 -> _void))
