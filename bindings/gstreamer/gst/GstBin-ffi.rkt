#lang at-exp racket

(require "gst_base.rkt"
         "gst-structs-ffi.rkt"
         "GstElement-ffi.rkt")

(provide (all-defined-out))





#|typedef enum {
  /* padding */
  GST_BIN_FLAG_LAST		= (GST_ELEMENT_FLAG_LAST << 5)
} GstBinFlags;|#

(define GST_BIN_FLAG_LAST (arithmetic-shift GST_ELEMENT_FLAG_LAST 5))



 #|(provide/doc


 (proc-doc/names gst_bin_new
                 (-> string? GstElement-pointer?)
                 (s)
                 @{Returns a new bin with name v.}))|#


;GstElement*         gst_bin_new                         (const gchar *name);
(define-gstreamer gst_bin_new (_fun _string -> _GstElement-pointer))



;GstBin* GstElement* -> gboolean
(define-gstreamer*
  (_fun _GstBin-pointer _GstElement-pointer -> _gboolean)
  gst_bin_add gst_bin_remove)

;GstBin* gchar* -> GstElement*
(define-gstreamer*
  (_fun _GstBin-pointer _string -> _GstElement-pointer)
  gst_bin_get_by_name gst_bin_get_by_name_recurse_up)

;GstElement*         gst_bin_get_by_interface            (GstBin *bin, GType iface);
(define-gstreamer gst_bin_get_by_interface (_fun _GstBin-pointer _GType -> _GstElement-pointer))

;GstBin* -> GstIterator*
(define-gstreamer*
  (_fun _GstBin-pointer -> _GstIterator-pointer)
  gst_bin_iterate_elements gst_bin_iterate_recurse gst_bin_iterate_sinks gst_bin_iterate_sorted gst_bin_iterate_sources)

;GstIterator*        gst_bin_iterate_all_by_interface    (GstBin *bin, GType iface);
(define-gstreamer gst_bin_iterate_all_by_interface (_fun _GstBin-pointer _GType -> _GstIterator-pointer))

;gboolean            gst_bin_recalculate_latency         (GstBin *bin);
(define-gstreamer gst_bin_recalculate_latency (_fun _GstBin-pointer -> _gboolean))

;void funcName(GstBin *bin, GstElement *element_1,...);
;GstBin* GstElement* ... -> void

(define (gst_bin_add_many bin . args)
  (for/list ([e args])
    (gst_bin_add bin e)))

(define (gst_bin_remove_many bin . args)
  (for/list ([e args])
    (gst_bin_remove bin e)))

;GstPad *            gst_bin_find_unlinked_pad           (GstBin *bin, GstPadDirection direction);
(define-gstreamer gst_bin_find_unlinked_pad (_fun  _GstBin-pointer _int -> _GstPad-pointer))
