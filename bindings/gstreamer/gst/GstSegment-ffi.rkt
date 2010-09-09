#lang racket

(require "gst_base.rkt"
         "GstFormat-ffi.rkt"
         "GstEvent-ffi.rkt")

(provide (all-defined-out))

#|
typedef struct {
  gdouble        rate;
  gdouble        abs_rate;
  GstFormat      format;
  GstSeekFlags   flags;
  gint64         start;
  gint64         stop;
  gint64         time;
  gint64         accum;
  gint64         last_stop;
  gint64         duration;

  /* API added 0.10.6 */
  gdouble        applied_rate;
} GstSegment;
|#

(define-cstruct _GstSegment
  ([rate _gdouble]
   [abs_rate _gdouble]
   [format _GstFormat]
   [flags _int]
   [start _gint64]
   [stop _gint64]
   [time _gint64]
   [accum _gint64]
   [last_stop _gint64]
   [duration _gint64]
   [applied_rate _gdouble]))


;gboolean            gst_segment_clip                    (GstSegment *segment, GstFormat format, gint64 start, gint64 stop, gint64 *clip_start, gint64 *clip_stop);
(define-gstreamer gst_segment_clip (_fun _GstSegment-pointer _GstFormat _gint64 _gint64 (_ptr io _gint64) (_ptr io _gint64) -> _gboolean))

;void                gst_segment_init                    (GstSegment *segment, GstFormat format);
(define-gstreamer gst_segment_init (_fun _GstSegment-pointer _GstFormat -> _void))

;GstSegment *        gst_segment_new                     (void);
(define-gstreamer gst_segment_new (_fun -> _GstSegment-pointer))

;GstSegment *        gst_segment_copy                    (GstSegment *segment);
(define-gstreamer gst_segment_copy (_fun _GstSegment-pointer -> _GstSegment-pointer))

;void                gst_segment_free                    (GstSegment *segment);
(define-gstreamer gst_segment_free (_fun _GstSegment-pointer -> _void))

;;GstSegment* GstFormat gint64 -> void
(define-gstreamer*
  (_fun _GstSegment-pointer _GstFormat _gint64 -> _void)
  gst_segment_set_duration gst_segment_set_last_stop)

;void                gst_segment_set_newsegment          (GstSegment *segment, gboolean update, gdouble rate, GstFormat format, gint64 start, gint64 stop, gint64 time);
(define-gstreamer gst_segment_set_newsegment (_fun _GstSegment-pointer _gboolean _gdouble _GstFormat _gint64 _gint64 _gint64 -> _void))

;void  gst_segment_set_newsegment_full (GstSegment *segment, gboolean update, gdouble rate, gdouble applied_rate, GstFormat format, gint64 start, gint64 stop, gint64 time);
(define-gstreamer gst_segment_set_newsegment_full (_fun _GstSegment-pointer _gboolean _gdouble _gdouble _GstFormat _gint64 _gint64 _gint64 -> _void))

;void gst_segment_set_seek (GstSegment *segment, gdouble rate, GstFormat format, GstSeekFlags flags, GstSeekType start_type, gint64 start, GstSeekType stop_type, gint64 stop, gboolean *update);
(define-gstreamer gst_segment_set_seek (_fun _GstSegment-pointer _gdouble _GstFormat _int _GstSeekType _gint64 _GstSeekType _gint64 (_ptr io _gboolean) -> _void))

;;GstSegment* GstFormat gint64 -> gint64
(define-gstreamer*
  (_fun _GstSegment-pointer _GstFormat _gint64 -> _gint64)
  gst_segment_to_running_time gst_segment_to_stream_time gst_segment_to_position)

;gboolean            gst_segment_set_running_time        (GstSegment *segment, GstFormat format, gint64 running_time);
(define-gstreamer gst_segment_set_running_time (_fun _GstSegment-pointer _GstFormat _gint64 -> _gboolean))
