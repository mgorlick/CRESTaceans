#lang racket

(require ffi/unsafe)
(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))


;GstTrace*           gst_trace_new                       (const gchar *filename, gint size);
(define-gstreamer gst_trace_new (_fun _string _gint -> _GstTrace-pointer))

;;GstTrace* -> void
(define-gstreamer*
  (_fun _GstTrace-pointer -> _void)
  gst_trace_destroy gst_trace_flush gst_trace_text_flush gst_trace_set_default)

;void                gst_trace_read_tsc                  (gint64 *dst);
(define-gstreamer gst_trace_read_tsc (_fun _gint64 -> _void))


#|typedef enum {
  GST_ALLOC_TRACE_LIVE		= (1 << 0),
  GST_ALLOC_TRACE_MEM_LIVE = (1 << 1)
} GstAllocTraceFlags;|#

(define GST_ALLOC_TRACE_LIVE (arithmetic-shift 1 0))
(define GST_ALLOC_TRACE_MEM_LIVE (arithmetic-shift 1 1))

  
;gboolean            gst_alloc_trace_available           (void);
(define-gstreamer gst_alloc_trace_available (_fun -> _gboolean))

;const GList*        gst_alloc_trace_list                (void);
(define-gstreamer gst_alloc_trace_list (_fun -> _GList-pointer))

;int                 gst_alloc_trace_live_all            (void);
(define-gstreamer gst_alloc_trace_live_all (_fun -> _int))

;;void -> void
(define-gstreamer*
  (_fun -> _void)
  gst_alloc_trace_print_all gst_alloc_trace_print_live)

;void                gst_alloc_trace_set_flags_all       (GstAllocTraceFlags flags);
(define-gstreamer gst_alloc_trace_set_flags_all (_fun _int -> _void))

;GstAllocTrace*      gst_alloc_trace_get                 (const gchar *name);
(define-gstreamer gst_alloc_trace_get (_fun _string -> _GstAllocTrace-pointer))

;void                gst_alloc_trace_print               (const GstAllocTrace *trace);
(define-gstreamer gst_alloc_trace_print (_fun _GstAllocTrace-pointer -> _void))

;void                gst_alloc_trace_set_flags           (GstAllocTrace *trace, GstAllocTraceFlags flags);
(define-gstreamer gst_alloc_trace_set_flags (_fun _GstAllocTrace-pointer _int -> _void))

#|#define             gst_trace_add_entry                 (trace, seq, data, msg)
#define             gst_trace_get_size                  (trace)
#define             gst_trace_get_offset                (trace)
#define             gst_trace_get_remaining             (trace)
#define             gst_alloc_trace_register            (name)
#define             gst_alloc_trace_new                 (trace, mem)
#define             gst_alloc_trace_free                (trace, mem)|#
