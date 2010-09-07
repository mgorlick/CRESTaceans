#! /usr/bin/racket
#lang racket

(require "gstreamer.rkt")


;gst_init(NULL, NULL);
(gst_init #f #f)


;GstElement *pipeline = gst_element_factory_make("playbin", "player");
(let ((pipeline (gst_element_factory_make "playbin" "player")))
  
  ;g_object_set(G_OBJECT(pipeline), "uri", uri, NULL);
  ;(g_object_set pipeline "uri" '("file:///home/Alegria/Documents/Racket-Gstreamer/GST/song.mp3"))
  (g_object_set pipeline "uri" (list "location" #f))
  
  ;gst_element_set_state(GST_ELEMENT(pipeline), GST_STATE_PLAYING);
  (gst_element_set_state pipeline GST_STATE_PLAYING)
  
  ;GMainLoop *loop = g_main_loop_new(NULL, FALSE);
  (let ((loop (g_main_loop_new #f 0)))
    
    ;g_main_loop_run(loop);
    (g_main_loop_run loop)
    
    
    
    ;gst_element_set_state(GST_ELEMENT(pipeline), GST_STATE_NULL);
    (gst_element_set_state pipeline GST_STATE_NULL)
    
    ;gst_object_unref(GST_OBJECT(pipeline));
    (gst_object_unref pipeline)
  
  (display "hello")))


