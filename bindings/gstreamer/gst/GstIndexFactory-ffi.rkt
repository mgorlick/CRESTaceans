#lang racket

(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))


;GstIndexFactory*    gst_index_factory_new               (const gchar *name, const gchar *longdesc, GType type);
(define-gstreamer gst_index_factory_new (_fun _string _string _GType -> _GstIndexFactory-pointer))

;void                gst_index_factory_destroy           (GstIndexFactory *factory);
(define-gstreamer gst_index_factory_destroy (_fun _GstIndexFactory-pointer -> _void))

;GstIndexFactory*    gst_index_factory_find              (const gchar *name);
(define-gstreamer gst_index_factory_find (_fun _string -> _GstIndexFactory-pointer))

;GstIndex*           gst_index_factory_create            (GstIndexFactory *factory);
(define-gstreamer gst_index_factory_create (_fun _GstIndexFactory-pointer -> _GstIndex-pointer))

;GstIndex*           gst_index_factory_make              (const gchar *name);
(define-gstreamer gst_index_factory_make (_fun _string -> _GstIndex-pointer))
