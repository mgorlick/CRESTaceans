#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         "../bindings/gstreamer.rkt")
(provide signal_connect
         on_pad_added
         cast_proc_to_gcallback
         bus_call
         )

(define test1-wrap (ffi-lib "/usr/local/lib/libracketgst-test1" "1.0"))
(define-ffi-definer df test1-wrap)

(define _type_on_pad_added (_fun _GstElement-pointer _GstPad-pointer _gpointer -> _void))
(df on_pad_added _type_on_pad_added)
(df cast_proc_to_gcallback (_fun _type_on_pad_added -> _GCallback))

(df bus_call (_fun _GstBus-pointer _GstMessage-pointer _gpointer -> _gboolean))

(df signal_connect (_fun _GstElement-pointer _GstElement-pointer -> _void))