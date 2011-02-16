#! /usr/bin/env racket
#lang racket

(require "../gstreamer.rkt"
         "util.rkt")

(with-gst-init
 #f
 (let-values ([(bin error) (gst_parse_launch "filesrc location=sample.ogg ! oggdemux ! udpsink host=127.0.0.1 port=44000")])
   (gst_element_set_state bin GST_STATE_PLAYING)
   (std-event-loop bin)
   (gst_element_set_state bin GST_STATE_NULL)
   (gst_object_unref bin)))