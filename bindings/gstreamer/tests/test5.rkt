#! /usr/bin/racket
#lang racket

(require ffi/unsafe
         "../gstreamer.rkt"
         "common-wrap/wrap.rkt")

; shows use of the parse_launch functionality, 
; plus an in-thread event loop

(define (event-loop pipe)
  (let ([bus (gst_element_get_bus pipe)])
    (call/cc 
     (lambda (k)
       (let loop ([cc k])
         (let* ([message (gst_bus_poll bus GST_MESSAGE_ANY -1)]
                [type (gst_message_type message)])
           (cond
             [(eq? type GST_MESSAGE_EOS)
              (printf "eos~n")
              (cc #t)
              (gst_message_unref_w message)
              ]
             [(or (eq? type GST_MESSAGE_WARNING)
                  (eq? type GST_MESSAGE_ERROR))
              (printf "error~n")
              (let ([debug-str (malloc _string 'raw)])
                (let-values ([(error) (gst_message_parse_error message debug-str)])
                  (gst_object_default_error (cast message _GstMessage-pointer _GstObject-pointer) 
                                            error (ptr-ref debug-str _string))
                  (g_error_free error)
                  )
                (free debug-str))
              (gst_message_unref_w message)
              (cc #f)]
             [else 
              (printf "else: ~s~n" type)
              (gst_message_unref_w message)
              (loop cc)]
             )))))))

(define (parselaunch arg1)
  (with-gst-init
   (list arg1)
   ;(let-values ([(bin error) ; version that plays mp3 successfully
   ;              (gst_parse_launch "filesrc name=my_filesrc ! mad ! audioconvert ! audioresample ! osssink")])
   (let-values ([(bin error) ; version that generates error
                 (gst_parse_launch "filesrc name=my_filesrc ! mad ! osssink")]) 
     (let ([filesrc (gst_bin_get_by_name (cast bin _GstElement-pointer _GstBin-pointer) "my_filesrc")])
       (g_object_set filesrc "location" arg1)
       (gst_element_set_state bin GST_STATE_PLAYING)
       (event-loop bin)
       (gst_element_set_state bin GST_STATE_NULL)
       (gst_object_unref filesrc))
     (gst_object_unref bin))))

(parselaunch "song.mp3")