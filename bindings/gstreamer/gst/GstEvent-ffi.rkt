#lang racket

(require "gst_base.rkt"
         "gst-structs-ffi.rkt"
         "GstClock-ffi.rkt")

(provide (all-defined-out))


#|typedef enum {
  GST_EVENT_UNKNOWN               = GST_EVENT_MAKE_TYPE (0, 0),
  /* bidirectional events */
  GST_EVENT_FLUSH_START           = GST_EVENT_MAKE_TYPE (1, FLAG(BOTH)),
  GST_EVENT_FLUSH_STOP            = GST_EVENT_MAKE_TYPE (2, FLAG(BOTH) | FLAG(SERIALIZED)),
  /* downstream serialized events */
  GST_EVENT_EOS                   = GST_EVENT_MAKE_TYPE (5, FLAG(DOWNSTREAM) | FLAG(SERIALIZED)),
  GST_EVENT_NEWSEGMENT            = GST_EVENT_MAKE_TYPE (6, FLAG(DOWNSTREAM) | FLAG(SERIALIZED)),
  GST_EVENT_TAG                   = GST_EVENT_MAKE_TYPE (7, FLAG(DOWNSTREAM) | FLAG(SERIALIZED)),
  GST_EVENT_BUFFERSIZE            = GST_EVENT_MAKE_TYPE (8, FLAG(DOWNSTREAM) | FLAG(SERIALIZED)),
  GST_EVENT_SINK_MESSAGE          = GST_EVENT_MAKE_TYPE (9, FLAG(DOWNSTREAM) | FLAG(SERIALIZED)),
  /* upstream events */
  GST_EVENT_QOS                   = GST_EVENT_MAKE_TYPE (15, FLAG(UPSTREAM)),
  GST_EVENT_SEEK                  = GST_EVENT_MAKE_TYPE (16, FLAG(UPSTREAM)),
  GST_EVENT_NAVIGATION            = GST_EVENT_MAKE_TYPE (17, FLAG(UPSTREAM)),
  GST_EVENT_LATENCY               = GST_EVENT_MAKE_TYPE (18, FLAG(UPSTREAM)),
  GST_EVENT_STEP                  = GST_EVENT_MAKE_TYPE (19, FLAG(UPSTREAM)),

  /* custom events start here */
  GST_EVENT_CUSTOM_UPSTREAM       = GST_EVENT_MAKE_TYPE (32, FLAG(UPSTREAM)),
  GST_EVENT_CUSTOM_DOWNSTREAM     = GST_EVENT_MAKE_TYPE (32, FLAG(DOWNSTREAM) | FLAG(SERIALIZED)),
  GST_EVENT_CUSTOM_DOWNSTREAM_OOB = GST_EVENT_MAKE_TYPE (32, FLAG(DOWNSTREAM)),
  GST_EVENT_CUSTOM_BOTH           = GST_EVENT_MAKE_TYPE (32, FLAG(BOTH) | FLAG(SERIALIZED)),
  GST_EVENT_CUSTOM_BOTH_OOB       = GST_EVENT_MAKE_TYPE (32, FLAG(BOTH))
} GstEventType;|#


#|
typedef enum {
  GST_EVENT_TYPE_UPSTREAM       = 1 << 0,
  GST_EVENT_TYPE_DOWNSTREAM     = 1 << 1,
  GST_EVENT_TYPE_SERIALIZED     = 1 << 2
} GstEventTypeFlags;|#

(define GST_EVENT_TYPE_UPSTREAM (arithmetic-shift 1 0))
(define GST_EVENT_TYPE_DOWNSTREAM (arithmetic-shift 1 1))
(define GST_EVENT_TYPE_SERIALIZED (arithmetic-shift 1 2))

(define GST_EVENT_TYPE_BOTH (bitwise-ior GST_EVENT_TYPE_UPSTREAM GST_EVENT_TYPE_DOWNSTREAM))

(define GstEventTypeShift 4)

(define (GEMT num flags)
  (bitwise-ior flags (arithmetic-shift num GstEventTypeShift)))

(define GST_EVENT_UNKNOWN 0)
(define GST_EVENT_FLUSH_START 19)
(define GST_EVENT_FLUSH_STOP 39)
(define GST_EVENT_EOS 86)
(define GST_EVENT_NEWSEGMENT 102)
(define GST_EVENT_TAG 118)
(define GST_EVENT_BUFFERSIZE 134)
(define GST_EVENT_SINK_MESSAGE  150)
(define GST_EVENT_QOS 241)
(define GST_EVENT_SEEK 257)
(define GST_EVENT_NAVIGATION 273)  
(define GST_EVENT_LATENCY 289)
(define GST_EVENT_STEP 305)
(define GST_EVENT_CUSTOM_UPSTREAM 513)
(define GST_EVENT_CUSTOM_DOWNSTREAM 518)
(define GST_EVENT_CUSTOM_DOWNSTREAM_OOB 514)
(define GST_EVENT_CUSTOM_BOTH 519)
(define GST_EVENT_CUSTOM_BOTH_OOB 515)


;(GEMT 0 0)
;(GEMT 1 GST_EVENT_TYPE_BOTH)
;(GEMT 2 (bitwise-ior GST_EVENT_TYPE_BOTH GST_EVENT_TYPE_SERIALIZED))
;
;(GEMT 5 (bitwise-ior GST_EVENT_TYPE_DOWNSTREAM GST_EVENT_TYPE_SERIALIZED))
;(GEMT 6 (bitwise-ior GST_EVENT_TYPE_DOWNSTREAM GST_EVENT_TYPE_SERIALIZED))
;(GEMT 7 (bitwise-ior GST_EVENT_TYPE_DOWNSTREAM GST_EVENT_TYPE_SERIALIZED))
;(GEMT 8 (bitwise-ior GST_EVENT_TYPE_DOWNSTREAM GST_EVENT_TYPE_SERIALIZED))
;(GEMT 9 (bitwise-ior GST_EVENT_TYPE_DOWNSTREAM GST_EVENT_TYPE_SERIALIZED))
;
;(GEMT 15 GST_EVENT_TYPE_UPSTREAM)
;(GEMT 16 GST_EVENT_TYPE_UPSTREAM)
;(GEMT 17 GST_EVENT_TYPE_UPSTREAM)
;(GEMT 18 GST_EVENT_TYPE_UPSTREAM)
;(GEMT 19 GST_EVENT_TYPE_UPSTREAM)
;
;(GEMT 32 GST_EVENT_TYPE_UPSTREAM)
;(GEMT 32 (bitwise-ior GST_EVENT_TYPE_DOWNSTREAM GST_EVENT_TYPE_SERIALIZED))
;(GEMT 32 GST_EVENT_TYPE_DOWNSTREAM)
;(GEMT 32 (bitwise-ior GST_EVENT_TYPE_BOTH GST_EVENT_TYPE_SERIALIZED))
;(GEMT 32 GST_EVENT_TYPE_BOTH)


#|#define             GST_EVENT_TRACE_NAME
#define             GST_EVENT_TYPE                      (event)
#define             GST_EVENT_TYPE_NAME                 (event)
#define             GST_EVENT_TIMESTAMP                 (event)
#define             GST_EVENT_SRC                       (event)
#define             GST_EVENT_IS_UPSTREAM               (ev)
#define             GST_EVENT_IS_DOWNSTREAM             (ev)
#define             GST_EVENT_IS_SERIALIZED             (ev)
#define             gst_event_replace                   (old_event, new_event)|#



;GstEventTypeFlags   gst_event_type_get_flags            (GstEventType type);
(define-gstreamer gst_event_type_get_flags (_fun _int -> _int))

;const gchar*        gst_event_type_get_name             (GstEventType type);
(define-gstreamer gst_event_type_get_name (_fun _int -> _string))

;GQuark              gst_event_type_to_quark             (GstEventType type);
(define-gstreamer gst_event_type_to_quark (_fun _int -> _GQuark))

;void                gst_event_unref                     (GstEvent *event);
;(define-gstreamer gst_event_unref (_fun _GstEvent-pointer -> _void)) ;;NOT IN LIB

;GstEvent*           gst_event_new_custom                (GstEventType type, GstStructure *structure);
(define-gstreamer gst_event_new_custom (_fun _int _GstStructure-pointer -> _GstEvent-pointer))

;const GstStructure * gst_event_get_structure            (GstEvent *event);
(define-gstreamer gst_event_get_structure (_fun _GstEvent-pointer -> _GstStructure-pointer))

;gboolean            gst_event_has_name                  (GstEvent *event, const gchar *name);
(define-gstreamer gst_event_has_name (_fun _GstEvent-pointer _string -> _gboolean))

;guint32             gst_event_get_seqnum                (GstEvent *event);
(define-gstreamer gst_event_get_seqnum (_fun _GstEvent-pointer -> _guint32))

;void                gst_event_set_seqnum                (GstEvent *event, guint32 seqnum);
(define-gstreamer gst_event_set_seqnum (_fun _GstEvent-pointer _guint32 -> _void))

;;void -> GstEvent*
(define-gstreamer*
  (_fun -> _GstEvent-pointer)
  gst_event_new_flush_start gst_event_new_flush_stop gst_event_new_eos)


;GstEvent*           gst_event_new_new_segment           (gboolean update, gdouble rate, GstFormat format, gint64 start, gint64 stop, gint64 position);
(define-gstreamer gst_event_new_new_segment (_fun _gboolean _gdouble _int _gint64 _gint64 _gint64 -> _GstEvent-pointer))

;GstEvent*           gst_event_new_new_segment_full      (gboolean update, gdouble rate, gdouble applied_rate, GstFormat format, gint64 start, gint64 stop, gint64 position);
(define-gstreamer gst_event_new_new_segment_full (_fun _gboolean _gdouble _gdouble _int _gint64 _gint64 _gint64 -> _GstEvent-pointer))

;void                gst_event_parse_new_segment         (GstEvent *event, gboolean *update, gdouble *rate, GstFormat *format, gint64 *start, gint64 *stop, gint64 *position);
(define-gstreamer gst_event_parse_new_segment (_fun _GstEvent-pointer (_ptr io _gboolean) (_ptr io _gdouble) (_ptr io _int) (_ptr io _gint64) (_ptr io _gint64) (_ptr io _gint64) -> _void))

;void gst_event_parse_new_segment_full (GstEvent *event, gboolean *update, gdouble *rate, gdouble *applied_rate, GstFormat *format, gint64 *start, gint64 *stop, gint64 *position);
(define-gstreamer gst_event_parse_new_segment_full (_fun _GstEvent-pointer (_ptr io _gboolean) (_ptr io _gdouble) (_ptr io _gdouble) (_ptr io _int) (_ptr io _gint64) (_ptr io _gint64) (_ptr io _gint64) -> _void))

;GstEvent*           gst_event_new_tag                   (GstTagList *taglist);
(define-gstreamer gst_event_new_tag (_fun _GstTagList-pointer -> _GstEvent-pointer))

;void                gst_event_parse_tag                 (GstEvent *event, GstTagList **taglist);
(define-gstreamer gst_event_parse_tag (_fun _GstEvent-pointer (_ptr io _GstTagList-pointer) -> _void))

;GstEvent *          gst_event_new_buffer_size           (GstFormat format, gint64 minsize, gint64 maxsize, gboolean async);
(define-gstreamer gst_event_new_buffer_size (_fun _int _gint64 _gint _gboolean -> _GstEvent-pointer))

;void                gst_event_parse_buffer_size         (GstEvent *event, GstFormat *format, gint64 *minsize, gint64 *maxsize, gboolean *async);
(define-gstreamer gst_event_parse_buffer_size (_fun _GstEvent-pointer (_ptr io _int) (_ptr io _gint64) (_ptr io _gint) (_ptr io _gboolean) -> _void))

;GstEvent*           gst_event_new_qos                   (gdouble proportion, GstClockTimeDiff diff, GstClockTime timestamp);
(define-gstreamer gst_event_new_qos (_fun _gdouble _GstClockTimeDiff _GstClockTime -> _GstEvent-pointer))

;void                gst_event_parse_qos                 (GstEvent *event, gdouble *proportion, GstClockTimeDiff *diff, GstClockTime *timestamp);
(define-gstreamer gst_event_parse_qos (_fun _GstEvent-pointer (_ptr io _gdouble) (_ptr io _GstClockTimeDiff) (_ptr io _GstClockTime) -> _void))

#|
typedef enum {
  /* one of these */
  GST_SEEK_TYPE_NONE            = 0,
  GST_SEEK_TYPE_CUR             = 1,
  GST_SEEK_TYPE_SET             = 2,
  GST_SEEK_TYPE_END             = 3
} GstSeekType;|#

(define GST_SEEK_TYPE_NONE 0)
(define GST_SEEK_TYPE_CUR 1) 
(define GST_SEEK_TYPE_SET 2) 
(define GST_SEEK_TYPE_END 3)

#|typedef enum {
  GST_SEEK_FLAG_NONE            = 0,
  GST_SEEK_FLAG_FLUSH           = (1 << 0),
  GST_SEEK_FLAG_ACCURATE        = (1 << 1),
  GST_SEEK_FLAG_KEY_UNIT        = (1 << 2),
  GST_SEEK_FLAG_SEGMENT         = (1 << 3),
  GST_SEEK_FLAG_SKIP            = (1 << 4)
} GstSeekFlags;|#

(define GST_SEEK_FLAG_NONE 0)
(define GST_SEEK_FLAG_FLUSH (arithmetic-shift 1 0))
(define GST_SEEK_FLAG_ACCURATE (arithmetic-shift 1 1))
(define GST_SEEK_FLAG_KEY_UNIT (arithmetic-shift 1 2))
(define GST_SEEK_FLAG_SEGMENT (arithmetic-shift 1 3))
(define GST_SEEK_FLAG_SKIP (arithmetic-shift 1 4))


;GstEvent*           gst_event_new_seek (gdouble rate, GstFormat format, GstSeekFlags flags, GstSeekType start_type, gint64 start, GstSeekType stop_type, gint64 stop);
(define-gstreamer gst_event_new_seek (_fun _gdouble _int _int _int _gint64 _int _gint64 -> _GstEvent-pointer))

;void gst_event_parse_seek (GstEvent *event, gdouble *rate, GstFormat *format, GstSeekFlags *flags, GstSeekType *start_type, gint64 *start, GstSeekType *stop_type, gint64 *stop);
(define-gstreamer gst_event_parse_seek (_fun _GstEvent-pointer (_ptr io _gdouble) (_ptr io _int) (_ptr io _int) (_ptr io _int)  (_ptr io _gint64) (_ptr io _int) (_ptr io _gint64) -> _void))

;GstEvent*           gst_event_new_navigation            (GstStructure *structure);
(define-gstreamer gst_event_new_navigation (_fun _GstStructure-pointer -> _GstEvent-pointer))

;GstEvent*           gst_event_new_latency               (GstClockTime latency);
(define-gstreamer gst_event_new_latency (_fun _GstClockTime -> _GstEvent-pointer))

;void                gst_event_parse_latency             (GstEvent *event, GstClockTime *latency);
(define-gstreamer gst_event_parse_latency (_fun _GstEvent-pointer (_ptr io _GstClockTime) -> _void))

;GstEvent*           gst_event_new_step                  (GstFormat format, guint64 amount, gdouble rate, gboolean flush, gboolean intermediate);
(define-gstreamer gst_event_new_step (_fun _int _guint64 _gdouble _gboolean _gboolean -> _GstEvent-pointer))

;void                gst_event_parse_step                (GstEvent *event, GstFormat *format, guint64 *amount, gdouble *rate, gboolean *flush, gboolean *intermediate);
(define-gstreamer gst_event_parse_step (_fun _GstEvent-pointer (_ptr io _int) (_ptr io _guint64) (_ptr io _gdouble) (_ptr io _gboolean) (_ptr io _gboolean) -> _void))

;GstEvent*           gst_event_new_sink_message          (GstMessage *msg);
(define-gstreamer gst_event_new_sink_message (_fun _GstMessage-pointer -> _GstEvent-pointer))

;void                gst_event_parse_sink_message        (GstEvent *event, GstMessage **msg);
(define-gstreamer gst_event_parse_sink_message (_fun _GstEvent-pointer (_ptr io _GstMessage-pointer) -> _void))
