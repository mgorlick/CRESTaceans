#! /usr/bin/racket
#lang racket

(require "../gst/gstreamer.rkt"
         "common-wrap/wrap.rkt")

(define path-to-file (string-append "file://" (path->string (current-directory))))
(define file-name "video.mpe")

(define (buscall bus* msg* data*)
  (let ([loop (cast data* _gpointer _GMainLoop-pointer)]
        [type (gst_message_type msg*)])
    (cond
      [(eq? type GST_MESSAGE_EOS) 
       (printf "End of stream~n") 
       (g_main_loop_quit loop)]
      [(eq? type GST_MESSAGE_ERROR)
       (printf "Unknown error~n")
       (g_main_loop_quit loop)]
      [else #f])
    1))

(define (print-position data*)
  (let ([fmt* (malloc _gint64 'raw)]
        [pos* (malloc _gint64 'raw)]
        [len* (malloc _gint64 'raw)]
        [pipeline* (cast data* _gpointer _GstElement-pointer)])
    (ptr-set! fmt* _int GST_FORMAT_TIME)
    (gst_element_query_position pipeline* fmt* pos*)
    (gst_element_query_duration pipeline* fmt* len*)
    (print_gst_time_format (ptr-ref pos* _gint64) (ptr-ref len* _gint64))
    (free fmt*)
    (free pos*)
    (free len*)
    )
  1)

(gst_init #f #f)

(let* ([pipeline (gst_element_factory_make "playbin2" "player")]
       [loop (g_main_loop_new #f 0)]
       [bus (gst_pipeline_get_bus (cast pipeline 
                                        _GstElement-pointer 
                                        _GstPipeline-pointer))])
  
  (g_object_set pipeline "uri" (string-append path-to-file file-name))
  (gst_bus_add_watch bus buscall loop)
  (gst_object_unref bus)
  (gst_element_set_state pipeline GST_STATE_PLAYING)
  (g_timeout_add 200 print-position pipeline)
  (g_main_loop_run loop)
  (gst_element_set_state pipeline GST_STATE_NULL)
  (gst_object_unref pipeline))