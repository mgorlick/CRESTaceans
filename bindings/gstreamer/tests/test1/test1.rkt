#! /usr/bin/racket
#lang racket

(require "../../gst/gstreamer.rkt"
         "test1-wrap.rkt"
         ffi/unsafe)

;; manual construction of ogg vorbis decode/demux pipeline

(let ([argc* (malloc _int 'raw)]
      [argv** (malloc (_list i _string) 'raw)])
  
  (ptr-set! argc* _int 1)
  (ptr-set! argv** (_list i _string) '("../sample.ogg"))
  (gst_init argc* argv**)
  
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
    
    (g_object_set_1 source "location" "../sample.ogg")
    
    ;(add_bus bus loop)
    (gst_bus_add_watch bus bus_call loop)
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
    (gst_object_unref pipeline)
    (free argc*)
    (free argv**)))
