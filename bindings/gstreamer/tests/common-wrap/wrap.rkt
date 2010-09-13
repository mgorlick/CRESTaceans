#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         "../../gst/gstreamer.rkt")
(provide (except-out (all-defined-out)
                     test1-wrap
                     df))

(define test1-wrap (ffi-lib "/usr/local/lib/libracketgst" "1.0"))
(define-ffi-definer df test1-wrap)

(df signal_connect (_fun _GstElement-pointer _GstElement-pointer -> _void))

(df gst_message_type (_fun _GstMessage-pointer -> _int))

(df print_gst_time_format (_fun _gint64 _gint64 -> _void))
