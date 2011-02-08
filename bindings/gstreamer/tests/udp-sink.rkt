#! /usr/bin/env racket
#lang racket

(require "../gstreamer.rkt")

(define (event-loop pipe)
  (let ([bus (gst_element_get_bus pipe)])
    (let loop ()
      (let* ([message (gst_bus_poll bus 'any -1)]
             [type (gst_message_type message)])
        (cond
          [(eq? type 'eos)
           (printf "eos~n")
           (gst_message_unref_w message)
           #t]
          [(or (eq? type 'warning)
               (eq? type 'error))
           (printf "error~n")
           (extract-and-print-error message)
           (gst_message_unref_w message)
           #f]
          [else 
           (printf "else: ~s~n" type)
           (gst_message_unref_w message)
           (loop)]
          )))))

(with-gst-init
 #f
 (let-values ([(bin error) (gst_parse_launch "audiotestsrc ! udpsink host=127.0.0.1 port=44000")])
   (gst_element_set_state bin GST_STATE_PLAYING)
   (event-loop bin)
   (gst_element_set_state bin GST_STATE_NULL)
   (gst_object_unref bin)))