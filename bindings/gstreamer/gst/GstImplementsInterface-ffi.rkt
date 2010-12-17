#lang racket

(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))


;gboolean            gst_element_implements_interface    (GstElement *element, GType iface_type);
(define-gstreamer gst_element_implements_interface (_fun _GstElement-pointer _GType -> _gboolean))

;gpointer            gst_implements_interface_cast       (gpointer from, GType type);
(define-gstreamer gst_implements_interface_cast (_fun _gpointer _GType -> _gpointer))

;gboolean            gst_implements_interface_check      (gpointer from, GType type);
(define-gstreamer gst_implements_interface_check (_fun _gpointer _GType -> _gboolean))
