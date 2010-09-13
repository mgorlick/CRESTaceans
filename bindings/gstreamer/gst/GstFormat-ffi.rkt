#lang racket

(require "gst_base.rkt"
         "GstStructs-ffi.rkt")

(provide (all-defined-out))

#|typedef enum {
  GST_FORMAT_UNDEFINED 	=  0, /* must be first in list */
  GST_FORMAT_DEFAULT   	=  1,
  GST_FORMAT_BYTES   	=  2,
  GST_FORMAT_TIME 	=  3,
  GST_FORMAT_BUFFERS =  4,
  GST_FORMAT_PERCENT =  5
} GstFormat;|#

; (define _GstFormat
;  (_enum '(GST_FORMAT_UNDEFINED = 0 GST_FORMAT_DEFAULT = 1 
;          GST_FORMAT_BYTES = 2 GST_FORMAT_TIME = 3 
;          GST_FORMAT_BUFFERS = 4 GST_FORMAT_PERCENT = 5)))

(define _GstFormat _int)
(define _GstFormat-pointer (_ptr io _int))

(define GST_FORMAT_UNDEFINED 0)
(define GST_FORMAT_DEFAULT 1)
(define GST_FORMAT_BYTES 2)
(define GST_FORMAT_TIME 3)
(define GST_FORMAT_BUFERS 4)
(define GST_FORMAT_PERCENT 5)

#|#define             GST_FORMAT_PERCENT_MAX
#define             GST_FORMAT_PERCENT_SCALE|#


#|typedef struct {
  GstFormat    value;
  const gchar *nick;
  const gchar *description;
  GQuark       quark;
} GstFormatDefinition;|#

(define-cstruct _GstFormatDefinition
  ([value _GstFormat]
   [nick _string]
   [description _string]
   [quark _GQuark]))

  
;const gchar*        gst_format_get_name                 (GstFormat format);
(define-gstreamer gst_format_get_name (_fun _GstFormat -> _string))

;GQuark              gst_format_to_quark                 (GstFormat format);
(define-gstreamer gst_format_to_quark (_fun _GstFormat -> _GQuark))

;GstFormat           gst_format_register                 (const gchar *nick, const gchar *description);
(define-gstreamer gst_format_register (_fun _string _string  -> _GstFormat))

;GstFormat           gst_format_get_by_nick              (const gchar *nick);
(define-gstreamer gst_format_get_by_nick (_fun _string -> _GstFormat))

;gboolean            gst_formats_contains                (const GstFormat *formats, GstFormat format);
(define-gstreamer gst_formats_contains (_fun (_ptr io _GstFormat) _GstFormat -> _gboolean))

;const GstFormatDefinition * gst_format_get_details      (GstFormat format);
(define-gstreamer gst_format_get_details (_fun _GstFormat -> _GstFormatDefinition-pointer))

;GstIterator*        gst_format_iterate_definitions      (void);
(define-gstreamer gst_format_iterate_definitions (_fun -> _GstIterator-pointer))
