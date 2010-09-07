#! /usr/bin/racket
#lang racket

(require "bindings/gstreamer.rkt"
         ffi/unsafe)

(define argc* (malloc _int 'raw))
(ptr-set! argc* _int 1)

(define argv** (malloc (_list i _string) 'raw))
(ptr-set! argv** (_list i _string) '("sample.ogg"))

(gst_init argc* argv**)

(let ((pipeline (gst_pipeline_new "audio-player"))
      (source (gst_element_factory_make "filesrc" "file-source"))
      (demuxer (gst_element_factory_make "oggdemux" "ogg-demuxer"))
      (decoder (gst_element_factory_make "vorbisdec" "vorbis-decoder"))
      (conv (gst_element_factory_make "audioconvert" "converter"))
      (sink (gst_element_factory_make "autoaudiosink" "audio-output")))
  
  (g_object_set source "location" "sample.ogg")
  (gst_bin_add (cast pipeline _GstElement-pointer _GstBin-pointer) source)
  (gst_bin_add (cast pipeline _GstElement-pointer _GstBin-pointer) demuxer)
  (gst_bin_add (cast pipeline _GstElement-pointer _GstBin-pointer) decoder)
  (gst_bin_add (cast pipeline _GstElement-pointer _GstBin-pointer) conv)
  (gst_bin_add (cast pipeline _GstElement-pointer _GstBin-pointer) sink)
  
  
  (printf "hello~n"))

(free argc*)
(free argv**)
               
               
         
  
#|(let ((pipeline (make "pipeline"))
        (filesrc (make "filesrc"))
        (oggdemux (make "oggdemux"))
        (vorbisdec (make "vorbisdec"))
        (audioconvert (make "audioconvert"))
        ;(alsasink (make "alsasink")) commented by A.
        (alsasink (make "autoaudiosink")))

  
   ; (define (pad-added element pad)
    ;  (link (pk pad) (get-pad vorbisdec "sink")))
 
    (add pipeline filesrc oggdemux vorbisdec audioconvert alsasink)
    (link filesrc oggdemux)
    (link vorbisdec audioconvert alsasink)

    (set filesrc 'location "sample.ogg")
    ;(connect oggdemux 'pad-added pad-added)

    (gst_element_set_state pipeline 'playing)
    (gst_bus_poll (get-bus pipeline) '(eos error) gparameter:uint64-max)
    (gst_element_set_state pipeline 'null))|#
  
  
