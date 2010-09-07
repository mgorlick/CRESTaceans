#lang racket

(require "gst_base.rkt"
         "GstClock-ffi.rkt")

(provide (all-defined-out))


;;typedef struct _GstSystemClock GstSystemClock;
(define-cpointer-type _GstSystemClock-pointer)

#|typedef enum {
  GST_CLOCK_TYPE_REALTIME       = 0,
  GST_CLOCK_TYPE_MONOTONIC      = 1
} GstClockType;|#

(define _GstClockType
  (_enum '(GST_CLOCK_TYPE_REALTIME = 0 GST_CLOCK_TYPE_MONOTONIC = 1)))

;GstClock*           gst_system_clock_obtain             (void);
(define-gstreamer gst_system_clock_obtain (_fun -> _GstClock-pointer))