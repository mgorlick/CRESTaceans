#! /usr/bin/env racket
#lang racket

(require ffi/unsafe
         "../gstreamer.rkt")

; shows use of the parse_launch functionality, 
; plus an in-thread event loop

(define (event-loop pipe)
  (let ([bus (gst_element_get_bus pipe)])
    (let loop ()
      (let* ([message (gst_bus_poll bus 'any -1)]
             [type (gst_message_type message)])
        (cond
          [(eq? type 'eos)
           (printf "eos~n")
           ;(gst_message_unref_w message)
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

(define (parselaunch arg1)
  (with-gst-init
   (list arg1)
   (let-values ([(bin error)
                 (gst_parse_launch "filesrc name=my_filesrc ! oggdemux ! vorbisdec ! audioconvert !  autoaudiosink")]) ; version that plays mp3 successfully
     ;(gst_parse_launch "filesrc name=my_filesrc ! mad ! osssink")]) ; version that generates error
     (let ([filesrc (gst_bin_get_by_name (cast bin _GstElement-pointer _GstBin-pointer) "my_filesrc")])
       (g_object_set filesrc "location" arg1)
       (gst_element_set_state bin GST_STATE_PLAYING)
       (event-loop bin)
       (gst_element_set_state bin GST_STATE_NULL)
       (gst_object_unref filesrc))
     (gst_object_unref bin))))

(parselaunch "sample.ogg")