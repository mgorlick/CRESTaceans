#lang racket

(require "gst_base.rkt"
         "GstStructs-ffi.rkt"
         "GstBin-ffi.rkt"
         "GstClock-ffi.rkt")

(provide (all-defined-out))

#|
typedef struct {
  GstClock      *fixed_clock;
  GstClockTime   stream_time;	
  GstClockTime   delay;
} GstPipeline;
|#

(define-cstruct _GstPipeline
  ([fixed_clock _GstClock-pointer]
   [stream_time _GstClockTime]
   [delay _GstClockTime]))


#|typedef enum {
  GST_PIPELINE_FLAG_FIXED_CLOCK        = (GST_BIN_FLAG_LAST << 0),
  /* padding */
  GST_PIPELINE_FLAG_LAST               = (GST_BIN_FLAG_LAST << 4)
} GstPipelineFlags;|#

(define GST_PIPELINE_FLAG_FIXED_CLOCK (arithmetic-shift GST_BIN_FLAG_LAST 0))
(define GST_PIPELINE_FLAG_LAST (arithmetic-shift GST_BIN_FLAG_LAST 4))


;GstElement*         gst_pipeline_new                    (const gchar *name);
(define-gstreamer gst_pipeline_new (_fun _string -> _GstElement-pointer))

;GstBus*             gst_pipeline_get_bus                (GstPipeline *pipeline);
(define-gstreamer gst_pipeline_get_bus (_fun _GstPipeline-pointer -> _GstBus-pointer))

;gboolean            gst_pipeline_set_clock              (GstPipeline *pipeline, GstClock *clock);
(define-gstreamer gst_pipeline_set_clock (_fun _GstPipeline-pointer _GstClock-pointer -> _gboolean))

;GstClock*           gst_pipeline_get_clock              (GstPipeline *pipeline);
(define-gstreamer gst_pipeline_get_clock (_fun _GstPipeline-pointer -> _GstClock-pointer))

;void                gst_pipeline_use_clock              (GstPipeline *pipeline, GstClock *clock);
(define-gstreamer gst_pipeline_use_clock (_fun _GstPipeline-pointer _GstClock-pointer -> _void))

;void                gst_pipeline_auto_clock             (GstPipeline *pipeline);
(define-gstreamer gst_pipeline_auto_clock (_fun _GstPipeline-pointer -> _void))

;void                gst_pipeline_set_new_stream_time    (GstPipeline *pipeline, GstClockTime time);
(define-gstreamer gst_pipeline_set_new_stream_time (_fun _GstPipeline-pointer _GstClockTime -> _void))

;GstClockTime        gst_pipeline_get_last_stream_time   (GstPipeline *pipeline);
(define-gstreamer gst_pipeline_get_last_stream_time (_fun _GstPipeline-pointer -> _GstClockTime))

;GstClockTime        gst_pipeline_get_delay              (GstPipeline *pipeline);
(define-gstreamer gst_pipeline_get_delay (_fun _GstPipeline-pointer -> _GstClockTime))

;void                gst_pipeline_set_auto_flush_bus     (GstPipeline *pipeline, gboolean auto_flush);
(define-gstreamer gst_pipeline_set_auto_flush_bus (_fun _GstPipeline-pointer _gboolean -> _void))

;gboolean            gst_pipeline_get_auto_flush_bus     (GstPipeline *pipeline);
(define-gstreamer gst_pipeline_get_auto_flush_bus (_fun _GstPipeline-pointer -> _gboolean))

;void                gst_pipeline_set_delay              (GstPipeline *pipeline, GstClockTime delay);
(define-gstreamer gst_pipeline_set_delay (_fun _GstPipeline-pointer _GstClockTime -> _void))

