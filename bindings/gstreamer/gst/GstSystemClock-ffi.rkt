#lang racket

(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))


#|typedef enum {
  GST_CLOCK_TYPE_REALTIME       = 0,
  GST_CLOCK_TYPE_MONOTONIC      = 1
} GstClockType;|#

(define GST_CLOCK_TYPE_REALTIME 0)
(define GST_CLOCK_TYPE_MONOTONIC 1)

;GstClock*           gst_system_clock_obtain             (void);
(define-gstreamer gst_system_clock_obtain (_fun -> _GstClock-pointer))