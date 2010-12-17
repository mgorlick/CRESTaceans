#lang racket

(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))

#|typedef enum {
  GST_FORMAT_UNDEFINED 	=  0, /* must be first in list */
  GST_FORMAT_DEFAULT   	=  1,
  GST_FORMAT_BYTES   	=  2,
  GST_FORMAT_TIME 	=  3,
  GST_FORMAT_BUFFERS =  4,
  GST_FORMAT_PERCENT =  5
} GstFormat;|#

(define GST_FORMAT_UNDEFINED 0)
(define GST_FORMAT_DEFAULT 1)
(define GST_FORMAT_BYTES 2)
(define GST_FORMAT_TIME 3)
(define GST_FORMAT_BUFERS 4)
(define GST_FORMAT_PERCENT 5)


#|#define             GST_FORMAT_PERCENT_MAX
#define             GST_FORMAT_PERCENT_SCALE|#
  
;const gchar*        gst_format_get_name                 (GstFormat format);
(define-gstreamer gst_format_get_name (_fun _int -> _string))

;GQuark              gst_format_to_quark                 (GstFormat format);
(define-gstreamer gst_format_to_quark (_fun _int -> _GQuark))

;GstFormat           gst_format_register                 (const gchar *nick, const gchar *description);
(define-gstreamer gst_format_register (_fun _string _string  -> _int))

;GstFormat           gst_format_get_by_nick              (const gchar *nick);
(define-gstreamer gst_format_get_by_nick (_fun _string -> _int))

;gboolean            gst_formats_contains                (const GstFormat *formats, GstFormat format);
(define-gstreamer gst_formats_contains (_fun (_ptr io _int) _int -> _gboolean))

;const GstFormatDefinition * gst_format_get_details      (GstFormat format);
(define-gstreamer gst_format_get_details (_fun _int -> _GstFormatDefinition-pointer))

;GstIterator*        gst_format_iterate_definitions      (void);
(define-gstreamer gst_format_iterate_definitions (_fun -> _GstIterator-pointer))
