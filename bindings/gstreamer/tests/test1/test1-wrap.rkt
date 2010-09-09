#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         "../../gst/gstreamer.rkt")
(provide signal_connect
         add_bus
         )

(define test1-wrap (ffi-lib "/usr/local/lib/libracketgst-test1" "1.0"))
(define-ffi-definer df test1-wrap)

(df signal_connect (_fun _GstElement-pointer _GstElement-pointer -> _void))
(df add_bus (_fun _GstBus-pointer _gpointer -> _guint))
