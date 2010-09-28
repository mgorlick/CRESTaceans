#! /usr/bin/env racket
#lang racket

(require ffi/unsafe
         "../gstreamer.rkt")
(provide (all-defined-out))

; port of the ghost pad example from gstreamer docs

(with-gst-init
 #f
 (let ([sink (gst_element_factory_make "fakesink" "sink")]
       [bin (cast (gst_bin_new "mybin") _GstElement-pointer _GstBin-pointer)])
   (gst_bin_add bin sink)
   (let ([pad (gst_element_get_static_pad sink "sink")])
     (gst_element_add_pad bin (gst_ghost_pad_new "sink" pad))
     (gst_object_unref pad))
   (gst_object_unref bin)))
