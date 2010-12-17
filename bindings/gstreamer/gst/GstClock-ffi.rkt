#lang racket

(require "gst_base.rkt"
         "gst-structs-ffi.rkt"
         "GstObject-ffi.rkt")

(provide (all-defined-out))


;;typedef guint64 GstClockTime;
(define _GstClockTime _guint64)

;;typedef gint64 GstClockTimeDiff;
(define _GstClockTimeDiff _gint64)

;;typedef gpointer GstClockID;
(define _GstClockID _gpointer)


;#define             GST_CLOCK_TIME_NONE
;#define             GST_CLOCK_TIME_IS_VALID             (time)
;#define             GST_SECOND
;#define             GST_MSECOND
;#define             GST_USECOND
;#define             GST_NSECOND
;#define             GST_TIME_AS_SECONDS                 (time)
;#define             GST_TIME_AS_MSECONDS                (time)
;#define             GST_TIME_AS_USECONDS                (time)
;#define             GST_TIME_AS_NSECONDS                (time)
;#define             GST_CLOCK_DIFF                      (s, e)
;#define             GST_TIMEVAL_TO_TIME                 (tv)
;#define             GST_TIME_TO_TIMEVAL                 (t, tv)
;#define             GST_TIMESPEC_TO_TIME                (ts)
;#define             GST_TIME_TO_TIMESPEC                (t, ts)
;#define             GST_CLOCK_ENTRY_TRACE_NAME          GstClockEntry;

;;gboolean            (*GstClockCallback)                 (GstClock *clock, GstClockTime time, GstClockID id, gpointer user_data);
(define GstClockCallback (_cprocedure (list _GstClock-pointer _GstClockTime _GstClockID _gpointer) _gboolean))


#|typedef enum {
  GST_CLOCK_ENTRY_SINGLE,
  GST_CLOCK_ENTRY_PERIODIC
} GstClockEntryType;|#

(define GST_CLOCK_ENTRY_SINGLE 0)
(define GST_CLOCK_ENTRY_PERIODIC 1)


;#define             GST_CLOCK_ENTRY                     (entry)

;#define             GST_CLOCK_ENTRY_CLOCK               (entry)

;#define             GST_CLOCK_ENTRY_TYPE                (entry)

;#define             GST_CLOCK_ENTRY_TIME                (entry)

;#define             GST_CLOCK_ENTRY_INTERVAL            (entry)

;#define             GST_CLOCK_ENTRY_STATUS              (entry)


#|typedef enum
{
  GST_CLOCK_OK		=  0,
  GST_CLOCK_EARLY =  1,
  GST_CLOCK_UNSCHEDULED =  2,
  GST_CLOCK_BUSY =  3,
  GST_CLOCK_BADTIME =  4,
  GST_CLOCK_ERROR =  5,
  GST_CLOCK_UNSUPPORTED =  6
} GstClockReturn;|#

(define GST_CLOCK_OK 0)
(define GST_CLOCK_EARLY 1)
(define GST_CLOCK_UNSCHEDULED 2)
(define GST_CLOCK_BUSY 3)
(define GST_CLOCK_BADTIME 4)
(define GST_CLOCK_ERROR 5)
(define GST_CLOCK_UNSUPPORTED 6)


#|typedef enum {
  GST_CLOCK_FLAG_CAN_DO_SINGLE_SYNC     = (GST_OBJECT_FLAG_LAST << 0),
  GST_CLOCK_FLAG_CAN_DO_SINGLE_ASYNC    = (GST_OBJECT_FLAG_LAST << 1),
  GST_CLOCK_FLAG_CAN_DO_PERIODIC_SYNC   = (GST_OBJECT_FLAG_LAST << 2),
  GST_CLOCK_FLAG_CAN_DO_PERIODIC_ASYNC  = (GST_OBJECT_FLAG_LAST << 3),
  GST_CLOCK_FLAG_CAN_SET_RESOLUTION     = (GST_OBJECT_FLAG_LAST << 4),
  GST_CLOCK_FLAG_CAN_SET_MASTER         = (GST_OBJECT_FLAG_LAST << 5),
  /* padding */
  GST_CLOCK_FLAG_LAST		        = (GST_OBJECT_FLAG_LAST << 8)
} GstClockFlags;|#

(define GST_CLOCK_FLAG_CAN_DO_SINGLE_SYNC (arithmetic-shift GST_OBJECT_FLAG_LAST 0))
(define GST_CLOCK_FLAG_CAN_DO_SINGLE_ASYNC (arithmetic-shift GST_OBJECT_FLAG_LAST 1))
(define GST_CLOCK_FLAG_CAN_DO_PERIODIC_SYNC (arithmetic-shift GST_OBJECT_FLAG_LAST 2))
(define GST_CLOCK_FLAG_CAN_DO_PERIODIC_ASYNC (arithmetic-shift GST_OBJECT_FLAG_LAST 3))
(define GST_CLOCK_FLAG_CAN_SET_RESOLUTION (arithmetic-shift GST_OBJECT_FLAG_LAST 4))
(define GST_CLOCK_FLAG_CAN_SET_MASTER (arithmetic-shift GST_OBJECT_FLAG_LAST 5))
(define GST_CLOCK_FLAG_LAST (arithmetic-shift GST_OBJECT_FLAG_LAST 8))


;#define             GST_CLOCK_FLAGS                     (clock)

;#define             GST_CLOCK_BROADCAST                 (clock)

;#define             GST_CLOCK_COND                      (clock)

;#define             GST_CLOCK_TIMED_WAIT                (clock, tv)

;#define             GST_CLOCK_WAIT                      (clock)

;gboolean            gst_clock_add_observation           (GstClock *clock, GstClockTime slave, GstClockTime master, gdouble *r_squared);
(define-gstreamer gst_clock_add_observation (_fun _GstClock-pointer _GstClockTime _GstClockTime (_ptr io _gdouble) -> _gboolean))

;gboolean            gst_clock_set_master                (GstClock *clock, GstClock *master);
(define-gstreamer gst_clock_set_master (_fun _GstClock-pointer _GstClock-pointer -> _gboolean))

;GstClock*           gst_clock_get_master                (GstClock *clock);
(define-gstreamer gst_clock_get_master (_fun _GstClock-pointer -> _GstClock-pointer))

;GstClockTime        gst_clock_set_resolution            (GstClock *clock, GstClockTime resolution);
(define-gstreamer gst_clock_set_resolution (_fun _GstClock-pointer _GstClockTime -> _GstClockTime))

;;GstClock* -> GstClockTime
(define-gstreamer*
  (_fun _GstClock-pointer -> _GstClockTime)
  gst_clock_get_resolution gst_clock_get_time)

;GstClockID          gst_clock_new_single_shot_id        (GstClock *clock, GstClockTime time);
(define-gstreamer gst_clock_new_single_shot_id (_fun _GstClock-pointer _GstClockTime -> _GstClockID))

;GstClockID          gst_clock_new_periodic_id           (GstClock *clock, GstClockTime start_time, GstClockTime interval);
(define-gstreamer gst_clock_new_periodic_id (_fun _GstClock-pointer _GstClockTime _GstClockTime -> _GstClockID))

;GstClockTime        gst_clock_get_internal_time         (GstClock *clock);
(define-gstreamer gst_clock_get_internal_time (_fun _GstClock-pointer -> _GstClockTime))

;;GstClock* GstClockTime -> GstClockTime
(define-gstreamer*
  (_fun _GstClock-pointer _GstClockTime -> _GstClockTime)
  gst_clock_adjust_unlocked gst_clock_unadjust_unlocked)

;void                gst_clock_get_calibration           (GstClock *clock, GstClockTime *internal, GstClockTime *external, GstClockTime *rate_num, GstClockTime *rate_denom);
(define-gstreamer gst_clock_get_calibration (_fun _GstClock-pointer (_ptr io _GstClockTime) (_ptr io _GstClockTime) (_ptr io _GstClockTime) (_ptr io _GstClockTime) -> _void))

;void                gst_clock_set_calibration           (GstClock *clock, GstClockTime internal, GstClockTime external, GstClockTime rate_num, GstClockTime rate_denom);
(define-gstreamer gst_clock_set_calibration (_fun _GstClock-pointer _GstClockTime _GstClockTime _GstClockTime _GstClockTime -> _void))

;GstClockTime        gst_clock_id_get_time               (GstClockID id);
(define-gstreamer gst_clock_id_get_time (_fun _GstClockID -> _GstClockTime))

;GstClockReturn      gst_clock_id_wait                   (GstClockID id, GstClockTimeDiff *jitter);
(define-gstreamer gst_clock_id_wait (_fun _GstClockID (_ptr io _GstClockTimeDiff) -> _int))

;GstClockReturn      gst_clock_id_wait_async             (GstClockID id, GstClockCallback func, gpointer user_data); OJO!!! depends on GstClockCallback
(define-gstreamer gst_clock_id_wait_async (_fun _GstClockID GstClockCallback _gpointer -> _int))

;;NOT IN LIB
;GstClockReturn gst_clock_id_wait_async_full (GstClockID id, GstClockCallback func, gpointer user_data, GDestroyNotify destroy_data); OJO!! depends on GstClockCallback
;(define-gstreamer gst_clock_id_wait_async_full (_fun _GstClockID GstClockCallback _gpointer _GDestroyNotify -> _int))

;;GstClock* GstClockTime -> GstClockTime
(define-gstreamer*
  (_fun _GstClockID -> _void)
  gst_clock_id_unschedule gst_clock_id_unref)

;gint                gst_clock_id_compare_func           (gconstpointer id1, gconstpointer id2);
(define-gstreamer gst_clock_id_compare_func (_fun _gconstpointer _gconstpointer -> _gint))

;GstClockID          gst_clock_id_ref                    (GstClockID id);
(define-gstreamer gst_clock_id_ref (_fun _GstClockID -> _GstClockID))







  