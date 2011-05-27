#! /usr/bin/env racket
#lang racket

(require "../gstreamer.rkt"
         "util.rkt")

(with-gst-init
 #f
 (let-values ([(bin error) (gst_parse_launch "udpsrc port=44000 ! vorbisdec ! audioconvert ! autoaudiosink")])
   (gst_element_set_state bin GST_STATE_PLAYING)
   (std-event-loop bin)
   (gst_element_set_state bin GST_STATE_NULL)
   (gst_object_unref bin)))