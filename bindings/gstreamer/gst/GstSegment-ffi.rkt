#lang racket

(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))



;gboolean            gst_segment_clip                    (GstSegment *segment, GstFormat format, gint64 start, gint64 stop, gint64 *clip_start, gint64 *clip_stop);
(define-gstreamer gst_segment_clip (_fun _GstSegment-pointer _int _gint64 _gint64 (_ptr io _gint64) (_ptr io _gint64) -> _gboolean))

;void                gst_segment_init                    (GstSegment *segment, GstFormat format);
(define-gstreamer gst_segment_init (_fun _GstSegment-pointer _int -> _void))

;GstSegment *        gst_segment_new                     (void);
(define-gstreamer gst_segment_new (_fun -> _GstSegment-pointer))

;GstSegment *        gst_segment_copy                    (GstSegment *segment);
(define-gstreamer gst_segment_copy (_fun _GstSegment-pointer -> _GstSegment-pointer))

;void                gst_segment_free                    (GstSegment *segment);
(define-gstreamer gst_segment_free (_fun _GstSegment-pointer -> _void))

;;GstSegment* GstFormat gint64 -> void
(define-gstreamer*
  (_fun _GstSegment-pointer _int _gint64 -> _void)
  gst_segment_set_duration gst_segment_set_last_stop)

;void                gst_segment_set_newsegment          (GstSegment *segment, gboolean update, gdouble rate, GstFormat format, gint64 start, gint64 stop, gint64 time);
(define-gstreamer gst_segment_set_newsegment (_fun _GstSegment-pointer _gboolean _gdouble _int _gint64 _gint64 _gint64 -> _void))

;void  gst_segment_set_newsegment_full (GstSegment *segment, gboolean update, gdouble rate, gdouble applied_rate, GstFormat format, gint64 start, gint64 stop, gint64 time);
(define-gstreamer gst_segment_set_newsegment_full (_fun _GstSegment-pointer _gboolean _gdouble _gdouble _int _gint64 _gint64 _gint64 -> _void))

;void gst_segment_set_seek (GstSegment *segment, gdouble rate, GstFormat format, GstSeekFlags flags, GstSeekType start_type, gint64 start, GstSeekType stop_type, gint64 stop, gboolean *update);
(define-gstreamer gst_segment_set_seek (_fun _GstSegment-pointer _gdouble _int _int _int _gint64 _int _gint64 (_ptr io _gboolean) -> _void))

;;GstSegment* GstFormat gint64 -> gint64
(define-gstreamer*
  (_fun _GstSegment-pointer _int _gint64 -> _gint64)
  gst_segment_to_running_time gst_segment_to_stream_time gst_segment_to_position)

;gboolean            gst_segment_set_running_time        (GstSegment *segment, GstFormat format, gint64 running_time);
(define-gstreamer gst_segment_set_running_time (_fun _GstSegment-pointer _int _gint64 -> _gboolean))
