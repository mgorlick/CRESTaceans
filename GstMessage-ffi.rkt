#lang racket

(require "gst_base.rkt"
         "GstStructs-ffi.rkt"
         "GstClock-ffi.rkt"
         "GstFormat-ffi.rkt"
         "GstMiniObject-ffi.rkt"
         "GstObject-ffi.rkt"
         "GstQuery-ffi.rkt"
         "GstStructure-ffi.rkt"
         "GstTagList-ffi.rkt" )

(provide (all-defined-out))


#|typedef enum
{
  GST_MESSAGE_UNKNOWN           = 0,
  GST_MESSAGE_EOS               = (1 << 0),
  GST_MESSAGE_ERROR             = (1 << 1),
  GST_MESSAGE_WARNING           = (1 << 2),
  GST_MESSAGE_INFO              = (1 << 3),
  GST_MESSAGE_TAG               = (1 << 4),
  GST_MESSAGE_BUFFERING         = (1 << 5),
  GST_MESSAGE_STATE_CHANGED     = (1 << 6),
  GST_MESSAGE_STATE_DIRTY       = (1 << 7),
  GST_MESSAGE_STEP_DONE         = (1 << 8),
  GST_MESSAGE_CLOCK_PROVIDE     = (1 << 9),
  GST_MESSAGE_CLOCK_LOST        = (1 << 10),
  GST_MESSAGE_NEW_CLOCK         = (1 << 11),
  GST_MESSAGE_STRUCTURE_CHANGE  = (1 << 12),
  GST_MESSAGE_STREAM_STATUS     = (1 << 13),
  GST_MESSAGE_APPLICATION       = (1 << 14),
  GST_MESSAGE_ELEMENT           = (1 << 15),
  GST_MESSAGE_SEGMENT_START     = (1 << 16),
  GST_MESSAGE_SEGMENT_DONE      = (1 << 17),
  GST_MESSAGE_DURATION          = (1 << 18),
  GST_MESSAGE_LATENCY           = (1 << 19),
  GST_MESSAGE_ASYNC_START       = (1 << 20),
  GST_MESSAGE_ASYNC_DONE        = (1 << 21),
  GST_MESSAGE_REQUEST_STATE     = (1 << 22),
  GST_MESSAGE_STEP_START        = (1 << 23),
  GST_MESSAGE_QOS               = (1 << 24),
  GST_MESSAGE_ANY               = ~0
} GstMessageType;|#

(define GST_MESSAGE_UNKNOWN 0)
(define GST_MESSAGE_EOS (arithmetic-shift 1 0))
(define GST_MESSAGE_ERROR (arithmetic-shift 1 1))
(define GST_MESSAGE_WARNING (arithmetic-shift 1 2))
(define GST_MESSAGE_INFO (arithmetic-shift 1 3))
(define GST_MESSAGE_TAG (arithmetic-shift 1 4))
(define GST_MESSAGE_BUFFERING (arithmetic-shift 1 5))
(define GST_MESSAGE_STATE_CHANGED (arithmetic-shift 1 6))
(define GST_MESSAGE_STATE_DIRTY (arithmetic-shift 1 7))
(define GST_MESSAGE_STEP_DONE (arithmetic-shift 1 8))
(define GST_MESSAGE_CLOCK_PROVIDE (arithmetic-shift 1 9))
(define GST_MESSAGE_CLOCK_LOST (arithmetic-shift 1 10))
(define GST_MESSAGE_NEW_CLOCK (arithmetic-shift 1 11))
(define GST_MESSAGE_STRUCTURE_CHANGE (arithmetic-shift 1 12))
(define GST_MESSAGE_STREAM_STATUS (arithmetic-shift 1 13))
(define GST_MESSAGE_APPLICATION (arithmetic-shift 1 14))
(define GST_MESSAGE_ELEMENT (arithmetic-shift 1 15))
(define GST_MESSAGE_SEGMENT_START (arithmetic-shift 1 16))
(define GST_MESSAGE_SEGMENT_DONE (arithmetic-shift 1 17))
(define GST_MESSAGE_DURATION (arithmetic-shift 1 18))
(define GST_MESSAGE_LATENCY (arithmetic-shift 1 19))
(define GST_MESSAGE_ASYNC_START (arithmetic-shift 1 20))
(define GST_MESSAGE_ASYNC_DONE (arithmetic-shift 1 21))
(define GST_MESSAGE_REQUEST_STATE (arithmetic-shift 1 22))
(define GST_MESSAGE_STEP_START (arithmetic-shift 1 23))
(define GST_MESSAGE_QOS (arithmetic-shift 1 24))
(define GST_MESSAGE_ANY (bitwise-not 0))


#|
typedef struct {
  GstMiniObject mini_object;
  GstMessageType type;
  guint64 timestamp;
  GstObject *src;
  GstStructure *structure;
} GstMessage;
|#

(define-cstruct _GstMessage
  ([mini_object _GstMiniObject]
   [type _int]
   [timestamp _guint64]
   [src _GstObject-pointer]
   [structure _GstStructure-pointer]))




#|#define             GST_MESSAGE_SRC                     (message)
#define             GST_MESSAGE_SRC_NAME                (message)
#define             GST_MESSAGE_TIMESTAMP               (message)
#define             GST_MESSAGE_TYPE                    (message)
#define             GST_MESSAGE_TYPE_NAME               (message)
#define             GST_MESSAGE_TRACE_NAME
#define             gst_message_make_writable           (msg)|#

;GQuark              gst_message_type_to_quark           (GstMessageType type);
(define-gstreamer gst_message_type_to_quark (_fun _int -> _GQuark))

;const gchar*        gst_message_type_get_name           (GstMessageType type);
(define-gstreamer gst_message_type_get_name (_fun _int -> _string))

;void                gst_message_unref                   (GstMessage *msg);
;(define-gstreamer gst_message_unref (_fun _GstMessage-pointer -> _void))  NOT IN LIB

;const GstStructure * gst_message_get_structure          (GstMessage *message);
(define-gstreamer gst_message_get_structure (_fun _GstMessage-pointer -> _GstStructure-pointer))

;guint32             gst_message_get_seqnum              (GstMessage *message);
(define-gstreamer gst_message_get_seqnum (_fun _GstMessage-pointer -> _guint32))

;void                gst_message_set_seqnum              (GstMessage *message, guint32 seqnum);
(define-gstreamer gst_message_set_seqnum (_fun _GstMessage-pointer _guint32 -> _void))

;;GstObject* -> GstMessage*
(define-gstreamer*
  (_fun _GstObject-pointer -> _GstMessage-pointer)
  gst_message_new_eos gst_message_new_state_dirty gst_message_new_latency)

;;GstObject* GError* gchar* -> GstMessage*
(define-gstreamer*
  (_fun _GstObject-pointer _GError-pointer _string -> _GstMessage-pointer)
  gst_message_new_error gst_message_new_warning gst_message_new_info)

;;GstMessage* GError** gchar** -> GstMessage*
(define-gstreamer*
  (_fun _GstMessage-pointer (_ptr io _GError-pointer) (_ptr io _string) -> _void)
  gst_message_parse_error gst_message_parse_warning gst_message_parse_info)


;GstMessage *        gst_message_new_tag                 (GstObject *src, GstTagList *tag_list);
(define-gstreamer gst_message_new_tag (_fun _GstObject-pointer _GstTagList-pointer -> _GstMessage-pointer))

;GstMessage *        gst_message_new_tag_full            (GstObject *src, GstPad *pad, GstTagList *tag_list);
(define-gstreamer gst_message_new_tag_full (_fun _GstObject-pointer _GstPad-pointer _GstTagList-pointer -> _GstMessage-pointer))

;void                gst_message_parse_tag               (GstMessage *message, GstTagList **tag_list);
(define-gstreamer gst_message_parse_tag (_fun _GstMessage-pointer (_ptr io _GstTagList-pointer) -> _void))

;void                gst_message_parse_tag_full          (GstMessage *message, GstPad **pad, GstTagList **tag_list);
(define-gstreamer gst_message_parse_tag_full (_fun _GstMessage-pointer  (_ptr io _GstPad-pointer) (_ptr io _GstTagList-pointer) -> _void))

;GstMessage *        gst_message_new_buffering           (GstObject *src, gint percent);
(define-gstreamer gst_message_new_buffering (_fun _GstObject-pointer _gint -> _GstMessage-pointer))

;void                gst_message_parse_buffering         (GstMessage *message, gint *percent);
(define-gstreamer gst_message_parse_buffering (_fun _GstMessage-pointer _gintptr -> _void))

;void                gst_message_set_buffering_stats     (GstMessage *message, GstBufferingMode mode, gint avg_in, gint avg_out, gint64 buffering_left);
(define-gstreamer gst_message_set_buffering_stats (_fun _GstMessage-pointer _GstBufferingMode _gint _gint _gint64 -> _void))

;void                gst_message_parse_buffering_stats   (GstMessage *message, GstBufferingMode *mode, gint *avg_in, gint *avg_out, gint64 *buffering_left);
(define-gstreamer gst_message_parse_buffering_stats (_fun _GstMessage-pointer (_ptr io _GstBufferingMode) _gintptr _gintptr (_ptr io _gint64) -> _void))

;GstMessage *        gst_message_new_state_changed       (GstObject *src, GstState oldstate, GstState newstate,GstState pending);
(define-gstreamer gst_message_new_state_changed (_fun _GstObject-pointer _int _int _int -> _GstMessage-pointer))

;void                gst_message_parse_state_changed     (GstMessage *message, GstState *oldstate, GstState *newstate, GstState *pending);
(define-gstreamer gst_message_parse_state_changed (_fun _GstMessage-pointer (_ptr io _int) (_ptr io _int) (_ptr io _int) -> _void))

;GstMessage * gst_message_new_step_done (GstObject *src, GstFormat format, guint64 amount, gdouble rate, gboolean flush, gboolean intermediate, guint64 duration, gboolean eos);
(define-gstreamer gst_message_new_step_done (_fun _GstObject-pointer _GstFormat _guint64 _gdouble _gboolean _gboolean _guint64 _gboolean -> _GstMessage-pointer))

;void gst_message_parse_step_done (GstMessage *message, GstFormat *format, guint64 *amount, gdouble *rate, gboolean *flush, gboolean *intermediate, guint64 *duration, gboolean *eos);
(define-gstreamer gst_message_parse_step_done (_fun _GstMessage-pointer (_ptr io _GstFormat) (_ptr io _guint64) (_ptr io _gdouble) (_ptr io _gboolean) (_ptr io _gboolean) (_ptr io _guint64) (_ptr io _gboolean) -> _void))

;GstMessage *        gst_message_new_clock_provide       (GstObject *src, GstClock *clock, gboolean ready);
(define-gstreamer gst_message_new_clock_provide (_fun _GstObject-pointer _GstClock-pointer _gboolean -> _GstMessage-pointer))

;void                gst_message_parse_clock_provide     (GstMessage *message, GstClock **clock, gboolean *ready);
(define-gstreamer gst_message_parse_clock_provide (_fun _GstMessage-pointer (_ptr io _GstClock-pointer) (_ptr io _gboolean) -> _void))

;;GstObject* GstClock* -> GstMessage*
(define-gstreamer*
  (_fun _GstObject-pointer _GstClock-pointer -> _GstMessage-pointer)
  gst_message_new_clock_lost gst_message_new_new_clock)

;;GstMessage* GstClock** -> void
(define-gstreamer*
  (_fun _GstMessage-pointer (_ptr io _GstClock-pointer) -> _void)
  gst_message_parse_clock_lost gst_message_parse_new_clock)

;;GstObject* GstStructure* -> GstMessage*
(define-gstreamer*
  (_fun _GstObject-pointer _GstStructure-pointer -> _GstMessage-pointer)
  gst_message_new_application gst_message_new_element)

;GstMessage *        gst_message_new_custom              (GstMessageType type, GstObject *src, GstStructure *structure);
(define-gstreamer gst_message_new_custom (_fun _int _GstObject-pointer _GstStructure-pointer -> _GstMessage-pointer))

;GstMessage *        gst_message_new_segment_start       (GstObject *src, GstFormat format, gint64 position);
(define-gstreamer gst_message_new_segment_start (_fun _GstObject-pointer _GstFormat _gint64 -> _GstMessage-pointer))

;;GstMessage* GstFormat* gint64 -> void
(define-gstreamer*
  (_fun _GstMessage-pointer (_ptr io _GstFormat) _gint64 -> _void)
  gst_message_parse_segment_start gst_message_parse_segment_done gst_message_parse_duration)


;;GstObject* GstFormat gint64 -> GstMessage*
(define-gstreamer*
  (_fun _GstObject-pointer _GstFormat _gint64 -> _GstMessage-pointer)
  gst_message_new_segment_done gst_message_new_duration)

;GstMessage *        gst_message_new_async_start         (GstObject *src, gboolean new_base_time);
(define-gstreamer gst_message_new_async_start (_fun _GstObject-pointer _gboolean -> _GstMessage-pointer))

;void                gst_message_parse_async_start       (GstMessage *message, gboolean *new_base_time);
(define-gstreamer gst_message_parse_async_start (_fun _GstMessage-pointer (_ptr io _gboolean) -> _void))

;GstMessage *        gst_message_new_async_done          (GstObject *src);
(define-gstreamer gst_message_new_async_done (_fun _GstObject-pointer -> _GstMessage-pointer))

;GstMessage * gst_message_new_step_start (GstObject *src, gboolean active, GstFormat format, guint64 amount, gdouble rate, gboolean flush, gboolean intermediate);
(define-gstreamer gst_message_new_step_start (_fun _GstObject-pointer _gboolean _GstFormat _guint64 _gdouble _gboolean _gboolean -> _GstMessage-pointer))

;void gst_message_parse_step_start (GstMessage *message, gboolean *active, GstFormat *format, guint64 *amount, gdouble *rate, gboolean *flush, gboolean *intermediate);
(define-gstreamer gst_message_parse_step_start (_fun _GstMessage-pointer (_ptr io _gboolean) (_ptr io _GstFormat) (_ptr io _guint64) (_ptr io _gdouble) (_ptr io _gboolean) (_ptr io _gboolean)-> _void))

;GstMessage *        gst_message_new_qos                 (GstObject *src, gboolean live, guint64 running_time, guint64 stream_time, guint64 timestamp, guint64 duration);
;(define-gstreamer gst_message_new_qos (_fun _GstObject-pointer _gboolean _guint64 _guint64 _guint64 _guint64 -> _GstMessage-pointer)) NOT IN LIB

;void                gst_message_set_qos_values          (GstMessage *message, gint64 jitter, gdouble proportion, gint quality);
;(define-gstreamer gst_message_set_qos_values (_fun _GstMessage-pointer _gint64 _gdouble _gint -> _void)) NOT IN LIB 

;void                gst_message_set_qos_stats           (GstMessage *message, GstFormat format, guint64 processed, guint64 dropped);
;(define-gstreamer gst_message_set_qos_stats (_fun _GstMessage-pointer _GstFormat _guint64 _guint64 -> _void)) NOT IN LIB

;void                gst_message_parse_qos (GstMessage *message, gboolean *live, guint64 *running_time, guint64 *stream_time, guint64 *timestamp, guint64 *duration);
;(define-gstreamer gst_message_parse_qos (_fun _GstMessage-pointer (_ptr io _gboolean) (_ptr io _guint64) (_ptr io _guint64) (_ptr io _guint64) (_ptr io _guint64) -> _void)) NOT IN LIB

;void                gst_message_parse_qos_values        (GstMessage *message, gint64 *jitter, gdouble *proportion, gint *quality);
;(define-gstreamer gst_message_parse_qos_values (_fun _GstMessage-pointer (_ptr io _gint64) (_ptr io _gdouble) (_ptr io _gint) -> _void)) NOT IN LIB

;void                gst_message_parse_qos_stats         (GstMessage *message, GstFormat *format, guint64 *processed, guint64 *dropped);
;(define-gstreamer gst_message_parse_qos_stats (_fun _GstMessage-pointer (_ptr io _GstFormat) (_ptr io _guint64) (_ptr io _guint64) -> _void)) NOT IN LIB

#|typedef enum {
  GST_STRUCTURE_CHANGE_TYPE_PAD_LINK   = 0,
  GST_STRUCTURE_CHANGE_TYPE_PAD_UNLINK = 1
} GstStructureChangeType;|#

(define _GstStructureChangeType
  (_enum '(GST_STRUCTURE_CHANGE_TYPE_PAD_LINK = 0 GST_STRUCTURE_CHANGE_TYPE_PAD_UNLINK = 1)))


;GstMessage *        gst_message_new_structure_change    (GstObject *src, GstStructureChangeType type, GstElement *owner, gboolean busy);
(define-gstreamer gst_message_new_structure_change (_fun _GstObject-pointer _GstStructureChangeType _GstElement-pointer _gboolean -> _GstMessage-pointer))

;void                gst_message_parse_structure_change  (GstMessage *message, GstStructureChangeType *type, GstElement **owner, gboolean *busy);
(define-gstreamer gst_message_parse_structure_change (_fun _GstMessage-pointer (_ptr io _GstStructureChangeType) (_ptr io _GstElement-pointer) (_ptr io _gboolean) -> _void))

;GstMessage *        gst_message_new_request_state       (GstObject *src, GstState state);
(define-gstreamer gst_message_new_request_state (_fun _GstObject-pointer _int -> _GstMessage-pointer))
  
;void                gst_message_parse_request_state     (GstMessage *message, GstState *state);
(define-gstreamer gst_message_parse_request_state (_fun _GstMessage-pointer (_ptr io _int) -> _void))

#|typedef enum {
  GST_STREAM_STATUS_TYPE_CREATE   = 0,
  GST_STREAM_STATUS_TYPE_ENTER    = 1,
  GST_STREAM_STATUS_TYPE_LEAVE    = 2,
  GST_STREAM_STATUS_TYPE_DESTROY  = 3,

  GST_STREAM_STATUS_TYPE_START    = 8,
  GST_STREAM_STATUS_TYPE_PAUSE    = 9,
  GST_STREAM_STATUS_TYPE_STOP     = 10
} GstStreamStatusType;|#
  
(define _GstStreamStatusType
  (_enum '(GST_STREAM_STATUS_TYPE_CREATE = 0 GST_STREAM_STATUS_TYPE_ENTER = 1 GST_STREAM_STATUS_TYPE_LEAVE = 2 GST_STREAM_STATUS_TYPE_DESTROY = 3 GST_STREAM_STATUS_TYPE_START = 8 GST_STREAM_STATUS_TYPE_PAUSE = 9 GST_STREAM_STATUS_TYPE_STOP = 10)))

;GstMessage *        gst_message_new_stream_status       (GstObject *src, GstStreamStatusType type, GstElement *owner);
(define-gstreamer gst_message_new_stream_status (_fun _GstObject-pointer _GstStreamStatusType _GstElement-pointer -> _GstMessage-pointer))

;void                gst_message_parse_stream_status     (GstMessage *message, GstStreamStatusType *type, GstElement **owner);
(define-gstreamer gst_message_parse_stream_status (_fun _GstMessage-pointer (_ptr io _GstStreamStatusType) (_ptr io _GstElement-pointer) -> _void))

;void                gst_message_set_stream_status_object (GstMessage *message, const GValue *object);
(define-gstreamer gst_message_set_stream_status_object (_fun _GstMessage-pointer _GValue-pointer -> _void))

;const GValue *      gst_message_get_stream_status_object (GstMessage *message);
(define-gstreamer gst_message_get_stream_status_object (_fun _GstMessage-pointer -> _GValue-pointer))

