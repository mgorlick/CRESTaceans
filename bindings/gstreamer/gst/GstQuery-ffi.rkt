#lang racket

(require "gst_base.rkt"
         "gst-structs-ffi.rkt"
         "GstClock-ffi.rkt")

(provide (all-defined-out))


#|typedef enum {
  GST_BUFFERING_STREAM,
  GST_BUFFERING_DOWNLOAD,
  GST_BUFFERING_TIMESHIFT,
  GST_BUFFERING_LIVE
} GstBufferingMode;|#

(define GST_BUFFERING_STREAM 0)
(define GST_BUFFERING_DOWNLOAD 1)
(define GST_BUFFERING_TIMESHIFT 2)
(define GST_BUFFERING_LIVE 3)

#|typedef enum {
  GST_QUERY_NONE = 0,
  GST_QUERY_POSITION,
  GST_QUERY_DURATION,
  GST_QUERY_LATENCY,
  GST_QUERY_JITTER,     /* not in draft-query, necessary? */
  GST_QUERY_RATE,
  GST_QUERY_SEEKING,
  GST_QUERY_SEGMENT,
  GST_QUERY_CONVERT,
  GST_QUERY_FORMATS,
  GST_QUERY_BUFFERING,
  GST_QUERY_CUSTOM,
  GST_QUERY_URI
} GstQueryType;|#

(define GST_QUERY_NONE 0)
(define GST_QUERY_POSITION 1)
(define GST_QUERY_DURATION 2)
(define GST_QUERY_LATENCY 3)
(define GST_QUERY_JITTER 4)
(define GST_QUERY_RATE 5)
(define GST_QUERY_SEEKING 6)
(define GST_QUERY_SEGMENT 7)
(define GST_QUERY_CONVERT 8)
(define GST_QUERY_FORMATS 9)
(define GST_QUERY_BUFFERING 10)
(define GST_QUERY_CUSTOM 11)
(define GST_QUERY_URI 12)
  

#|#define             GST_QUERY_TYPE_NAME                 (query)
#define             gst_query_make_writable             (q)|#



;const gchar*        gst_query_type_get_name             (GstQueryType query);
(define-gstreamer gst_query_type_get_name (_fun _int -> _string))

;GQuark              gst_query_type_to_quark             (GstQueryType query);
(define-gstreamer gst_query_type_to_quark (_fun _int -> _GQuark))

;GstQueryType        gst_query_type_register             (const gchar *nick, const gchar *description);
(define-gstreamer gst_query_type_register (_fun _string _string -> _int))

;GstQueryType        gst_query_type_get_by_nick          (const gchar *nick);
(define-gstreamer gst_query_type_get_by_nick (_fun _string -> _int))

;gboolean            gst_query_types_contains            (const GstQueryType *types, GstQueryType type);
(define-gstreamer gst_query_types_contains (_fun (_ptr io _int) _int -> _gboolean))

;const GstQueryTypeDefinition * gst_query_type_get_details (GstQueryType type);
(define-gstreamer gst_query_type_get_details (_fun _int -> _GstQueryTypeDefinition-pointer))

;GstIterator*        gst_query_type_iterate_definitions  (void);
(define-gstreamer gst_query_type_iterate_definitions (_fun -> _GstIterator-pointer))

;void                gst_query_unref                     (GstQuery *q);
;(define-gstreamer gst_query_unref (_fun _GstQuery-pointer -> _void)) NOT IN LIB

;GstQuery *          gst_query_new_application           (GstQueryType type, GstStructure *structure);
(define-gstreamer gst_query_new_application (_fun _int _GstStructure-pointer -> _GstQuery-pointer))

;GstStructure *      gst_query_get_structure             (GstQuery *query);
(define-gstreamer gst_query_get_structure (_fun _GstQuery-pointer -> _GstStructure-pointer))

;GstQuery*           gst_query_new_convert               (GstFormat src_format, gint64 value, GstFormat dest_format);
(define-gstreamer gst_query_new_convert (_fun _int _gint64 _int -> _GstQuery-pointer))

;void                gst_query_set_convert               (GstQuery *query, GstFormat src_format, gint64 src_value, GstFormat dest_format, gint64 dest_value);
(define-gstreamer gst_query_set_convert (_fun _GstQuery-pointer _int _gint64 _int _gint64 -> _void))

;void                gst_query_parse_convert             (GstQuery *query, GstFormat *src_format, gint64 *src_value, GstFormat *dest_format, gint64 *dest_value);
(define-gstreamer gst_query_parse_convert (_fun _GstQuery-pointer (_ptr io _int) (_ptr io _gint64) (_ptr io _int) (_ptr io _gint64) -> _void))

;;GstFormat -> GstQuery*
(define-gstreamer*
  (_fun _int -> _GstQuery-pointer)
  gst_query_new_position gst_query_new_duration gst_query_new_seeking gst_query_new_segment gst_query_new_buffering)

;;GstQuery* GstFormat gint64 -> void
(define-gstreamer*
  (_fun _GstQuery-pointer _int _gint64 -> _void)
  gst_query_set_position gst_query_set_duration)

;;GstQuery* GstFormat* gint64* -> void
(define-gstreamer*
  (_fun _GstQuery-pointer (_ptr io _int) (_ptr io _gint64) -> _void)
  gst_query_parse_position gst_query_parse_duration)

;; void -> GstQuery*
(define-gstreamer*
  (_fun -> _GstQuery-pointer)
  gst_query_new_latency gst_query_new_formats gst_query_new_uri)

;void                gst_query_parse_latency             (GstQuery *query, gboolean *live, GstClockTime *min_latency, GstClockTime *max_latency);
(define-gstreamer gst_query_parse_latency (_fun _GstQuery-pointer (_ptr io _gboolean) (_ptr io _GstClockTime) (_ptr io _GstClockTime) -> _void))

;void                gst_query_set_latency               (GstQuery *query, gboolean live, GstClockTime min_latency, GstClockTime max_latency);
(define-gstreamer gst_query_set_latency (_fun _GstQuery-pointer _gboolean _GstClockTime _GstClockTime -> _void))

;void                gst_query_set_seeking               (GstQuery *query, GstFormat format, gboolean seekable, gint64 segment_start, gint64 segment_end);
(define-gstreamer gst_query_set_seeking (_fun _GstQuery-pointer _int _gboolean _gint64 _gint64 -> _void))

;void                gst_query_parse_seeking             (GstQuery *query, GstFormat *format, gboolean *seekable, gint64 *segment_start, gint64 *segment_end);
(define-gstreamer gst_query_parse_seeking (_fun _GstQuery-pointer (_ptr io _int) (_ptr io _gboolean) (_ptr io _gint64) (_ptr io _gint64) -> _void))

;void                gst_query_set_formats               (GstQuery *query, gint n_formats, ...);
(define-gstreamer gst_query_set_formats (_fun _GstQuery-pointer _gint (_list i _gint) -> _void))

;void                gst_query_set_formatsv              (GstQuery *query, gint n_formats, GstFormat *formats);
(define-gstreamer gst_query_set_formatsv (_fun _GstQuery-pointer _gint (_ptr io _int) -> _void))

;void                gst_query_parse_formats_length      (GstQuery *query, guint *n_formats);
(define-gstreamer gst_query_parse_formats_length (_fun _GstQuery-pointer (_ptr io _guint) -> _void))

;void                gst_query_parse_formats_nth         (GstQuery *query, guint nth, GstFormat *format);
(define-gstreamer gst_query_parse_formats_nth (_fun _GstQuery-pointer _guint (_ptr io _int) -> _void))

;void                gst_query_set_segment               (GstQuery *query, gdouble rate, GstFormat format, gint64 start_value, gint64 stop_value);
(define-gstreamer gst_query_set_segment (_fun _GstQuery-pointer _gdouble _int _gint64 _gint64 -> _void))

;void                gst_query_parse_segment             (GstQuery *query, gdouble *rate, GstFormat *format, gint64 *start_value, gint64 *stop_value);
(define-gstreamer gst_query_parse_segment (_fun _GstQuery-pointer (_ptr io _gdouble) (_ptr io _int) (_ptr io _gint64) (_ptr io _gint64) -> _void))

;void                gst_query_set_buffering_percent     (GstQuery *query, gboolean busy, gint percent);
(define-gstreamer gst_query_set_buffering_percent (_fun _GstQuery-pointer _gboolean _gint -> _void))

;void                gst_query_parse_buffering_percent   (GstQuery *query, gboolean *busy, gint *percent);
(define-gstreamer gst_query_parse_buffering_percent (_fun _GstQuery-pointer (_ptr io _gboolean) (_ptr io _gint) -> _void))

;void                gst_query_set_buffering_stats       (GstQuery *query, GstBufferingMode mode, gint avg_in, gint avg_out, gint64 buffering_left);
(define-gstreamer gst_query_set_buffering_stats (_fun _GstQuery-pointer _int _gint _gint _gint64 -> _void))

;void                gst_query_parse_buffering_stats     (GstQuery *query, GstBufferingMode *mode, gint *avg_in, gint *avg_out, gint64 *buffering_left);
(define-gstreamer gst_query_parse_buffering_stats (_fun _GstQuery-pointer (_ptr io _int) (_ptr io _gint) (_ptr io _gint) (_ptr io _gint64) -> _void))

;void                gst_query_set_buffering_range       (GstQuery *query, GstFormat format, gint64 start, gint64 stop, gint64 estimated_total);
(define-gstreamer gst_query_set_buffering_range (_fun _GstQuery-pointer _int _gint64 _gint64 _gint64 -> _void))

;void                gst_query_parse_buffering_range     (GstQuery *query, GstFormat *format, gint64 *start, gint64 *stop, gint64 *estimated_total);
(define-gstreamer gst_query_parse_buffering_range (_fun _GstQuery-pointer (_ptr io _int) (_ptr io _gint64) (_ptr io _gint64) (_ptr io _gint64) -> _void))

;void                gst_query_parse_uri                 (GstQuery *query, gchar **uri);
(define-gstreamer gst_query_parse_uri (_fun _GstQuery-pointer (_ptr io _string) -> _void))

;void                gst_query_set_uri                   (GstQuery *query, const gchar *uri);
(define-gstreamer gst_query_set_uri (_fun _GstQuery-pointer _string -> _void))