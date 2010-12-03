#lang racket

(require "gst_base.rkt")

(provide (all-defined-out))

;gboolean            (*GstFilterFunc)                    (gpointer obj, gpointer user_data);
(define GstFilterFunc (_cprocedure (list _gpointer _gpointer) _gboolean))

;GList*              gst_filter_run                      (const GList *list, GstFilterFunc func, gboolean first, gpointer user_data);
(define-gstreamer gst_filter_run (_fun _GList-pointer GstFilterFunc _gboolean _gpointer -> _GList-pointer))