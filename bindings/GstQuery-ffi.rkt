#lang racket

(require "gst_base.rkt"
         "GstStructs-ffi.rkt"
         "GstClock-ffi.rkt"
         "GstFormat-ffi.rkt"
         "GstMiniObject-ffi.rkt"
         "GstStructure-ffi.rkt")

(provide (all-defined-out))



#|typedef struct {
  GstQueryType   value;
  const gchar   *nick;
  const gchar   *description;
  GQuark         quark;
} GstQueryTypeDefinition;|#

(define-cstruct _GstQueryTypeDefinition
  ([value _GstQueryType]
   [nick _string]
   [description _string]
   [quark _GQuark]))


#|typedef enum {
  GST_BUFFERING_STREAM,
  GST_BUFFERING_DOWNLOAD,
  GST_BUFFERING_TIMESHIFT,
  GST_BUFFERING_LIVE
} GstBufferingMode;|#

(define _GstBufferingMode
  (_enum '(GST_BUFFERING_STREAM GST_BUFFERING_DOWNLOAD GST_BUFFERING_TIMESHIFT GST_BUFFERING_LIVE)))
  

#|#define             GST_QUERY_TYPE_NAME                 (query)
#define             gst_query_make_writable             (q)|#



;const gchar*        gst_query_type_get_name             (GstQueryType query);
(define-gstreamer gst_query_type_get_name (_fun _GstQueryType -> _string))

;GQuark              gst_query_type_to_quark             (GstQueryType query);
(define-gstreamer gst_query_type_to_quark (_fun _GstQueryType -> _GQuark))

;GstQueryType        gst_query_type_register             (const gchar *nick, const gchar *description);
(define-gstreamer gst_query_type_register (_fun _string _string -> _GstQueryType))

;GstQueryType        gst_query_type_get_by_nick          (const gchar *nick);
(define-gstreamer gst_query_type_get_by_nick (_fun _string -> _GstQueryType))

;gboolean            gst_query_types_contains            (const GstQueryType *types, GstQueryType type);
(define-gstreamer gst_query_types_contains (_fun (_ptr io _GstQueryType) _GstQueryType -> _gboolean))

;const GstQueryTypeDefinition * gst_query_type_get_details (GstQueryType type);
(define-gstreamer gst_query_type_get_details (_fun _GstQueryType -> _GstQueryTypeDefinition-pointer))

;GstIterator*        gst_query_type_iterate_definitions  (void);
(define-gstreamer gst_query_type_iterate_definitions (_fun -> _GstIterator-pointer))

;void                gst_query_unref                     (GstQuery *q);
;(define-gstreamer gst_query_unref (_fun _GstQuery-pointer -> _void)) NOT IN LIB

;GstQuery *          gst_query_new_application           (GstQueryType type, GstStructure *structure);
(define-gstreamer gst_query_new_application (_fun _GstQueryType _GstStructure-pointer -> _GstQuery-pointer))

;GstStructure *      gst_query_get_structure             (GstQuery *query);
(define-gstreamer gst_query_get_structure (_fun _GstQuery-pointer -> _GstStructure-pointer))

;GstQuery*           gst_query_new_convert               (GstFormat src_format, gint64 value, GstFormat dest_format);
(define-gstreamer gst_query_new_convert (_fun _GstFormat _gint64 _GstFormat -> _GstQuery-pointer))

;void                gst_query_set_convert               (GstQuery *query, GstFormat src_format, gint64 src_value, GstFormat dest_format, gint64 dest_value);
(define-gstreamer gst_query_set_convert (_fun _GstQuery-pointer _GstFormat _gint64 _GstFormat _gint64 -> _void))

;void                gst_query_parse_convert             (GstQuery *query, GstFormat *src_format, gint64 *src_value, GstFormat *dest_format, gint64 *dest_value);
(define-gstreamer gst_query_parse_convert (_fun _GstQuery-pointer (_ptr io _GstFormat) (_ptr io _gint64) (_ptr io _GstFormat) (_ptr io _gint64) -> _void))

;;GstFormat -> GstQuery*
(define-gstreamer*
  (_fun _GstFormat -> _GstQuery-pointer)
  gst_query_new_position gst_query_new_duration gst_query_new_seeking gst_query_new_segment gst_query_new_buffering)

;;GstQuery* GstFormat gint64 -> void
(define-gstreamer*
  (_fun _GstQuery-pointer _GstFormat _gint64 -> _void)
  gst_query_set_position gst_query_set_duration)

;;GstQuery* GstFormat* gint64* -> void
(define-gstreamer*
  (_fun _GstQuery-pointer (_ptr io _GstFormat) (_ptr io _gint64) -> _void)
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
(define-gstreamer gst_query_set_seeking (_fun _GstQuery-pointer _GstFormat _gboolean _gint64 _gint64 -> _void))

;void                gst_query_parse_seeking             (GstQuery *query, GstFormat *format, gboolean *seekable, gint64 *segment_start, gint64 *segment_end);
(define-gstreamer gst_query_parse_seeking (_fun _GstQuery-pointer (_ptr io _GstFormat) (_ptr io _gboolean) (_ptr io _gint64) (_ptr io _gint64) -> _void))

;void                gst_query_set_formats               (GstQuery *query, gint n_formats, ...);
(define-gstreamer gst_query_set_formats (_fun _GstQuery-pointer _gint (_list i _gint) -> _void))

;void                gst_query_set_formatsv              (GstQuery *query, gint n_formats, GstFormat *formats);
(define-gstreamer gst_query_set_formatsv (_fun _GstQuery-pointer _gint (_ptr io _GstFormat) -> _void))

;void                gst_query_parse_formats_length      (GstQuery *query, guint *n_formats);
(define-gstreamer gst_query_parse_formats_length (_fun _GstQuery-pointer (_ptr io _guint) -> _void))

;void                gst_query_parse_formats_nth         (GstQuery *query, guint nth, GstFormat *format);
(define-gstreamer gst_query_parse_formats_nth (_fun _GstQuery-pointer _guint (_ptr io _GstFormat) -> _void))

;void                gst_query_set_segment               (GstQuery *query, gdouble rate, GstFormat format, gint64 start_value, gint64 stop_value);
(define-gstreamer gst_query_set_segment (_fun _GstQuery-pointer _gdouble _GstFormat _gint64 _gint64 -> _void))

;void                gst_query_parse_segment             (GstQuery *query, gdouble *rate, GstFormat *format, gint64 *start_value, gint64 *stop_value);
(define-gstreamer gst_query_parse_segment (_fun _GstQuery-pointer (_ptr io _gdouble) (_ptr io _GstFormat) (_ptr io _gint64) (_ptr io _gint64) -> _void))

;void                gst_query_set_buffering_percent     (GstQuery *query, gboolean busy, gint percent);
(define-gstreamer gst_query_set_buffering_percent (_fun _GstQuery-pointer _gboolean _gint -> _void))

;void                gst_query_parse_buffering_percent   (GstQuery *query, gboolean *busy, gint *percent);
(define-gstreamer gst_query_parse_buffering_percent (_fun _GstQuery-pointer (_ptr io _gboolean) (_ptr io _gint) -> _void))

;void                gst_query_set_buffering_stats       (GstQuery *query, GstBufferingMode mode, gint avg_in, gint avg_out, gint64 buffering_left);
(define-gstreamer gst_query_set_buffering_stats (_fun _GstQuery-pointer _GstBufferingMode _gint _gint _gint64 -> _void))

;void                gst_query_parse_buffering_stats     (GstQuery *query, GstBufferingMode *mode, gint *avg_in, gint *avg_out, gint64 *buffering_left);
(define-gstreamer gst_query_parse_buffering_stats (_fun _GstQuery-pointer (_ptr io _GstBufferingMode) (_ptr io _gint) (_ptr io _gint) (_ptr io _gint64) -> _void))

;void                gst_query_set_buffering_range       (GstQuery *query, GstFormat format, gint64 start, gint64 stop, gint64 estimated_total);
(define-gstreamer gst_query_set_buffering_range (_fun _GstQuery-pointer _GstFormat _gint64 _gint64 _gint64 -> _void))

;void                gst_query_parse_buffering_range     (GstQuery *query, GstFormat *format, gint64 *start, gint64 *stop, gint64 *estimated_total);
(define-gstreamer gst_query_parse_buffering_range (_fun _GstQuery-pointer (_ptr io _GstFormat) (_ptr io _gint64) (_ptr io _gint64) (_ptr io _gint64) -> _void))

;void                gst_query_parse_uri                 (GstQuery *query, gchar **uri);
(define-gstreamer gst_query_parse_uri (_fun _GstQuery-pointer (_ptr io _string) -> _void))

;void                gst_query_set_uri                   (GstQuery *query, const gchar *uri);
(define-gstreamer gst_query_set_uri (_fun _GstQuery-pointer _string -> _void))