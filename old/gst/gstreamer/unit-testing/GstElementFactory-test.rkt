#lang racket

(require rackunit
         rackunit/text-ui
         "../gstreamer.rkt"
         ffi/unsafe
         srfi/13)

(provide (all-defined-out))

(define GstElementFactory-test
    (test-suite
     "Tests for GstElementFactory library"

     (test-case
      "Test finding an element factory"
      (with-gst-init #f
                     (let ([audiorate (gst_element_factory_find "audiorate")])
                       
                       (check-equal? (GstElementFactory-pointer? audiorate) #t))))
     
     
     (test-case
      "Test creating an element from a factory and from a factory name -  gst_element_factory_create and gst_element_factory_make"
      (with-gst-init #f
                     (let* ([audiorate-factory (gst_element_factory_find "audiorate")]
                           [audiorate (gst_element_factory_create audiorate-factory "audiorate")]
                           
                           [fakesrc (gst_element_factory_make "fakesrc" "fakesrc")])
                       
                       (check-equal? (GstElement-pointer? audiorate) #t)
                       (check-not-equal? (string-contains (get_gst_element_get_name audiorate) "audiorate") #f)
                       
                       (check-equal? (GstElement-pointer? fakesrc) #t)
                       (check-not-equal? (string-contains (get_gst_element_get_name fakesrc) "src") #f))))
     
     
     
      (test-case
      "Test getting an element's details - longname, author, description and klass"
      (with-gst-init #f
                     (let ([oggdemux (gst_element_factory_find "oggdemux")])
                       
                       (check-equal? (gst_element_factory_get_longname oggdemux) "Ogg demuxer")
                       
                       (check-equal? (gst_element_factory_get_author oggdemux) "Wim Taymans <wim@fluendo.com>")
                       
                       (check-equal? (gst_element_factory_get_description oggdemux) "demux ogg streams (info about ogg: http://xiph.org)")
                       
                       (check-equal? (gst_element_factory_get_klass oggdemux) "Codec/Demuxer")
                       
                       )))

      
      
     #| (test-case
      "Test getting an element's type"
      (with-gst-init #f
                     (let ([oggdemux (gst_element_factory_find "oggdemux")])
                       
                       ;Get the GType for elements managed by this factory. 
                       ;The type can only be retrieved if the element factory is loaded, which can be assured with gst_plugin_feature_load(). 
                       
                       (gst_plugin_feature_load ???)
                       (check-equal? (gst_element_factory_get_element_type oggdemux) ???))))|#

          
      
      (test-case
      "Test getting the number of pad templates from an element factory"
      (with-gst-init #f
                     
                     (let ([videorate (gst_element_factory_find "videorate")]
                           [fakesrc (gst_element_factory_find "fakesrc")])
                       
                       (check-equal? (gst_element_factory_get_num_pad_templates videorate) 2)
                       (check-equal? (gst_element_factory_get_num_pad_templates fakesrc) 1)
                       )))
      
      
      (test-case
      "Test getting the type and protocol of URIs the element supports"
      (with-gst-init #f
                     
                     (let ([videorate (gst_element_factory_find "videorate")])
                       
                       (check-equal? (gst_element_factory_get_uri_type videorate) 0)
                       
                       ;(check-equal? (gst_element_factory_get_uri_protocols videorate) "ad")
                       )))
      
      
      (test-case
      "Check whether a given factory implements an interface"
      (with-gst-init #f
                     
                     (let ([esdsink (gst_element_factory_find "esdsink")]
                           [autoaudiosink (gst_element_factory_find "autoaudiosink")])
                       
                       (check-equal? (gst_element_factory_has_interface esdsink "GstChildProxy") 0)
                       (check-equal? (gst_element_factory_has_interface autoaudiosink "GstChildProxy") 1))))
      
      
      
      (test-case
      "Checks if the factory can sink the given capability."
      (with-gst-init #f
                     (let ([esdsink (gst_element_factory_find "esdsink")]
                           [caps1 (gst_caps_from_string "audio/x-raw-int, endianness=byte_order, signed=(boolean)true, width=16, depth=16, rate=44100, channels=2")]
                           [caps2 (gst_caps_from_string "audio/mpeg, mpegversion=(int)1, layer=(int)3")])
                       
                       (check-equal? (gst_element_factory_can_sink_caps esdsink caps1) 1)
                       (check-equal? (gst_element_factory_can_sink_caps esdsink caps2) 0))))
      
      
      (test-case
      "Checks if the factory can src the given capability."
      (with-gst-init #f
                     (let ([alsasrc (gst_element_factory_find "alsasrc")]
                           [caps1 (gst_caps_from_string "audio/x-raw-int")]
                           [caps2 (gst_caps_from_string "audio/mpeg")])
                       
                       (check-equal? (gst_element_factory_can_src_caps alsasrc caps1) 1)
                       (check-equal? (gst_element_factory_can_src_caps alsasrc caps2) 0))))
      
      
      (test-case
      "Get the list of static pad templates from an element factory"
      (with-gst-init #f
                     (let* ([alsasrc (gst_element_factory_find "alsasrc")]
                           [template (gst_element_factory_get_static_pad_templates alsasrc)]
                           [first-element (g_list_nth_data template 0)])
                       
                       ;;alsasrc has no static pad templates. Check with a plugin that has.
                       (check-equal? (get_Gst_Pad_Template_Name_Template (gpointer-to-GstPadTemplatepPointer first-element)) #f))))
      
      
       ))