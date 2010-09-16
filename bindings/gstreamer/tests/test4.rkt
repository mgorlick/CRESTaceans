#! /usr/bin/racket
#lang racket

(require ffi/unsafe
         "test4/test4-wrap.rkt"
         "../gstreamer.rkt")
(provide (all-defined-out))

; port of the sometimes_available_pad example 

; (doesn't really do anything yet)

(with-gst-init
 '("sample.ogg")
  (let ([pipeline (gst_pipeline_new "my_pipeline")]
        [source (gst_element_factory_make "filesrc" "source")]
        [demuxer (gst_element_factory_make "oggdemux" "demuxer")]
        [loop (g_main_loop_new #f 0)])
    (g_object_set source "location" "sample.ogg")
    (gst_bin_add_many (cast pipeline _GstElement-pointer _GstBin-pointer) source demuxer)
    (gst_element_link_pads source "src" demuxer "sink")
    (t4_signal_connect demuxer)
    (gst_element_set_state pipeline GST_STATE_PLAYING)
    (g_main_loop_run loop)
    ))