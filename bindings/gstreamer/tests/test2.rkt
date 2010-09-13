#! /usr/bin/racket
#lang racket

(require "../gst/gstreamer.rkt"
         "test1/test1-wrap.rkt")

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

(gst_init #f #f)

(let* ([pipeline (gst_element_factory_make "playbin" "player")]
       [loop (g_main_loop_new #f 0)]
       [bus (gst_pipeline_get_bus (cast pipeline _GstElement-pointer _GstPipeline-pointer))])
  
  (g_object_set pipeline "uri" (string-append path-to-file file-name))
  (gst_bus_add_watch bus buscall loop)
  (gst_object_unref bus)
  (gst_element_set_state pipeline GST_STATE_PLAYING)
  (g_main_loop_run loop)
  (gst_element_set_state pipeline GST_STATE_NULL)
  (gst_object_unref pipeline))