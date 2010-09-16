#! /usr/bin/racket
#lang racket

(require "../gstreamer.rkt"
         "common-wrap/wrap.rkt"
         ffi/unsafe)

;; manual construction of ogg vorbis decode/demux pipeline

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


(with-gst-init 
 '("sample.ogg")
 (let* ((pipeline (gst_pipeline_new "audio-player"))
        (source (gst_element_factory_make "filesrc" "file-source"))
        (demuxer (gst_element_factory_make "oggdemux" "ogg-demuxer"))
        (decoder (gst_element_factory_make "vorbisdec" "vorbis-decoder"))
        (conv (gst_element_factory_make "audioconvert" "converter"))
        (sink (gst_element_factory_make "autoaudiosink" "audio-output"))
        [loop (g_main_loop_new #f 0)]
        [bus (gst_pipeline_get_bus (cast pipeline _GstElement-pointer _GstPipeline-pointer))])
   
   (if (or (null? pipeline) (null? source) (null? demuxer) (null? decoder)
           (null? conv) (null? sink) (null? loop))
       (printf "Pipeline could not be created~n")
       (printf "Pipeline elements created~n"))
   
   (g_object_set_1 source "location" "sample.ogg")
   
   (gst_bus_add_watch bus buscall loop)
   (gst_object_unref bus)
   
   (gst_bin_add (cast pipeline _GstElement-pointer _GstBin-pointer) source)
   (gst_bin_add (cast pipeline _GstElement-pointer _GstBin-pointer) demuxer)
   (gst_bin_add (cast pipeline _GstElement-pointer _GstBin-pointer) decoder)
   (gst_bin_add (cast pipeline _GstElement-pointer _GstBin-pointer) conv)
   (gst_bin_add (cast pipeline _GstElement-pointer _GstBin-pointer) sink)
   
   (gst_element_link source demuxer)
   (gst_element_link decoder conv)
   (gst_element_link conv sink)
   (printf "doing dynamic link~n")
   (signal_connect demuxer decoder)
   
   (gst_element_set_state pipeline GST_STATE_PLAYING)
   
   (printf "Running...~n")
   (g_main_loop_run loop)
   
   (printf "Returned, stopping playback~n")
   (gst_element_set_state pipeline GST_STATE_NULL)
   (printf "Deleting pipeline~n")
   (gst_object_unref pipeline)))