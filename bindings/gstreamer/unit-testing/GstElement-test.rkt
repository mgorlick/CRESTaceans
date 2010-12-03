#lang racket

(require rackunit
         rackunit/text-ui
         "../gstreamer.rkt"
         ffi/unsafe
         srfi/13)

(provide (all-defined-out))

(define GstElement-test
  (test-suite
   "Tests for GstElement library"
   
   
   (test-case
    "Test setting and getting an element's name."
    (with-gst-init #f
                   (let ([src (gst_element_factory_make "fakesrc" "source")])
                     
                     (check-equal? (get_gst_element_get_name src) "source")
                     
                     (set_gst_element_set_name src "src") 
                     (check-equal? (get_gst_element_get_name src) "src"))))
   
   
   (test-case
    "Test getting an element's factory name."
    (with-gst-init #f
                   (let* ([jpegenc-factory (gst_element_factory_find "jpegenc")]
                          [jpegenc (gst_element_factory_create jpegenc-factory "jpegenc")])
                     
                     (check-equal? (ptr-equal? (gst_element_get_factory jpegenc) jpegenc-factory) true))))
   
   
   
   (test-case
    "Test setting and getting the state of an element (can be pending, null, ready, paused or playing)."
    (with-gst-init #f
                   (let ([src (gst_element_factory_make "fakesrc" "source")])
                     
                     (check-equal? (get_Gst_State src) GST_STATE_NULL)
                     
                     ;(check-equal? (get_Gst_State_Next src) GST_STATE_READY) ;;not sure how to get something other than 0
                     
                     (gst_element_set_state src GST_STATE_PLAYING)                                              
                     (check-equal? (get_Gst_State src) GST_STATE_PLAYING) 
                     (check-equal? (gst_element_state_get_name (get_Gst_State src)) "PLAYING")
                     
                     (let-values ([(state pending) (gst_element_get_state src 1000)])
                       (check-equal? state GST_STATE_PLAYING) 
                       (check-equal? (gst_element_state_get_name state) "PLAYING")
                       (check-equal? pending GST_STATE_VOID_PENDING)))))
   
   
   (test-case
    "Test locking the state of an element so that changes in the parent element don't affect its state."
    (with-gst-init #f
                   (let ([src (gst_element_factory_make "fakesrc" "source")]
                         [bin (gst_bin_new "my-bin")])
                     
                     (gst_bin_add (elem-to-bin bin) src)
                     (check-equal? (gst_element_is_locked_state src) 0)
                     (check-equal? (gst_element_set_locked_state src 1) 1)
                     (check-equal? (gst_element_is_locked_state src) 1)
                     (check-equal? (gst_element_set_locked_state src 0) 1)
                     (check-equal? (gst_element_is_locked_state src) 0))))
   
   
   
   ;; NOT CHANGING STATE with gst_element_change_state
   #|(test-case
      "Change the state of an element, performing a transition on the element."
      ;This function must be called with STATE_LOCK held and is mainly used internally. 
      
      (with-gst-init #f
                     (let ([filesrc (gst_element_factory_make "filesrc" "filesrc")])

                       (g_object_set_1 filesrc "location" "sample.ogg")
                       (check-equal? (gst_element_set_locked_state filesrc 1) 1)
                       (check-equal? (get_Gst_State filesrc) GST_STATE_NULL)
                       (check-equal? (gst_element_change_state filesrc GST_STATE_CHANGE_NULL_TO_READY) 1)
                       (check-equal? (get_Gst_State filesrc) GST_STATE_READY))))|#
   
   
   (test-case
    "Test synchronizing the state of the element with the state of the parent."    
    (with-gst-init #f
                   (let ([bin (gst_bin_new "my-bin")]  
                         [vorbisenc (gst_element_factory_make "vorbisenc" "vorbisenc")])
                     
                     (gst_bin_add (elem-to-bin bin) vorbisenc)
                     
                     (check-equal? (get_Gst_State vorbisenc) GST_STATE_NULL)
                     (check-equal? (gst_element_set_state vorbisenc GST_STATE_PLAYING) 1)
                     (check-equal? (get_Gst_State vorbisenc) GST_STATE_PLAYING)
                     (check-equal? (get_Gst_State bin) GST_STATE_NULL)
                     (check-equal? (gst_element_sync_state_with_parent vorbisenc) 1)
                     (check-equal? (get_Gst_State vorbisenc) GST_STATE_NULL))))
   
   
   
   (test-case
    "Test returning the last GstStateChangeReturn value (the possible return values from a state change function)."
    (with-gst-init #f
                   (let ([src (gst_element_factory_make "fakesrc" "source")])
                     
                     (check-equal? (get_Gst_State src) GST_STATE_NULL)
                     (gst_element_set_state src GST_STATE_PLAYING)                       
                     (check-not-equal? (get_Gst_State_Return src) GST_STATE_CHANGE_FAILURE)
                     (check-equal? (get_Gst_State src) GST_STATE_PLAYING))))
   
   
   (test-case
    "Test returning the name of a GstStateChangeReturn value."
    (with-gst-init #f
                   (begin
                     (check-equal? (gst_element_state_change_return_get_name 0) "FAILURE")
                     (check-equal? (gst_element_state_change_return_get_name 1) "SUCCESS")
                     (check-equal? (gst_element_state_change_return_get_name 2) "ASYNC")
                     (check-equal? (gst_element_state_change_return_get_name 3) "NO PREROLL"))))
   
   
   
   (test-case
    "Test adding, removing and retrieving pads from an element."
    (with-gst-init #f
                   (let ([source (gst_element_factory_make "fakesrc" "source")]
                         [pad (gst_pad_new "source-pad" GST_PAD_SRC)])
                     
                     (check-equal? (GstPad-pointer? pad) #t)
                     
                     (check-equal? (gst_element_add_pad source pad) 1)
                     
                     (check-equal? (get_gst_pad_get_name (gst_element_get_static_pad source "source-pad")) "source-pad")
                     
                     (check-equal? (gst_element_remove_pad source pad) 1))))
   
   
   
   (test-case
    "Testing request pads. These pads are not created automatically but are only created on demand. This is very useful for multiplexers, aggregators and tee elements."
    ;; for more info: http://www.gstreamer.net/data/doc/gstreamer/head/manual/html/chapter-pads.html
    
    (with-gst-init #f
                   (let* ([tee (gst_element_factory_make "tee" "tee")]
                          [pad (gst_element_get_request_pad tee "src%d")])
                     
                     (check-equal? (get_gst_pad_get_name pad) "src0")
                     
                     (gst_element_release_request_pad tee pad))))
   
   
   
   (test-case
    "Testing returning an iterator for the element's pads (all, only sink or only src)."
    (with-gst-init #f
                   (let ([dvdsubparse (gst_element_factory_create (gst_element_factory_find "dvdsubparse") "dvdsubparse")])
                     
                     (check-equal? (GstIterator-pointer? (gst_element_iterate_pads dvdsubparse)) true)
                     (check-equal? (GstIterator-pointer? (gst_element_iterate_sink_pads dvdsubparse)) true)
                     (check-equal? (GstIterator-pointer? (gst_element_iterate_src_pads dvdsubparse)) true))))
   
   
   #|(test-case
      "Test request a pad that is compatible with another pad template."
      ;;This is very useful if you want to link an element to a multiplexer element and you need to request a pad that is compatible. 
      
      (with-gst-init #f
                     (let* ([tolink_pad (gst_pad_new "src-pad" GST_PAD_SRC)]
                           [mux (gst_element_factory_make "oggmux" "oggmux")]
                           ;;[pad (gst_element_get_compatible_pad mux tolink_pad #f)]  ;;NOT RETRIEVING ANY COMPATIBLE PAD
                           )
                       
                       (display "hello")
                       )))|#
   
   
   (test-case
    "Test linking elements directly. They'll get liked through their most obvious pads."
    (with-gst-init #f
                   (let* ([bin (gst_bin_new "my-bin")]
                          [audioconvert (gst_element_factory_make "audioconvert" "audioconvert")]  
                          [vorbisenc (gst_element_factory_make "vorbisenc" "vorbisenc")]
                          [fakesink (gst_element_factory_make "fakesink" "fakesink")])
                     
                     (gst_bin_add_many (elem-to-bin bin) audioconvert vorbisenc fakesink)
                     
                     (check-equal? (gst_element_link vorbisenc fakesink) 1)
                     
                     (gst_element_unlink vorbisenc fakesink)
                     
                     (check-equal? (gst_element_link_many (list audioconvert vorbisenc fakesink)) 1)
                     
                     (gst_element_unlink_many (list audioconvert vorbisenc fakesink)) ;test!
                     )))
   
   
   
   (test-case
    "Test linking elements by their pads."
    (with-gst-init #f
                   (let ([bin (gst_bin_new "my-bin1")]
                         [source (gst_element_factory_make "fakesrc" "source")]
                         [sink (gst_element_factory_make "fakesink" "sink")])
                     
                     (gst_bin_add_many (elem-to-bin bin) source sink)
                     
                     (check-equal? (gst_element_link_pads source "src" sink "sink") 1)
                     
                     (gst_element_unlink_pads source "src" sink "sink")))) ;test!
   
   
   (test-case
    "Test that linking elements (by their pad) that are in different bins fails."
    (with-gst-init #f
                   (let ([bin1 (gst_bin_new "my-bin1")]
                         [bin2 (gst_bin_new "my-bin2")]
                         [source (gst_element_factory_make "fakesrc" "source")]
                         [sink (gst_element_factory_make "fakesink" "sink")])
                     
                     (gst_bin_add (elem-to-bin bin1) source)
                     (gst_bin_add (elem-to-bin bin2) sink)
                     
                     ;(check-equal? (gst_element_link_pads source "src" sink "sink") 0) ;;WILL TRINGGER A WARNING, THAT'S EXPECTED
                     ))) 
   
   
   (test-case
    "Test linking src to dest using the given caps as filtercaps."
    (with-gst-init #f
                   (let ([bin (gst_bin_new "my-bin")]
                         [audiokaraoke (gst_element_factory_make "audiokaraoke" "audiokaraoke")]
                         [audioamplify (gst_element_factory_make "audioamplify" "audioamplify")]
                         [caps (gst_caps_from_string "audio/x-raw-float")])
                     
                     
                     (gst_bin_add_many (elem-to-bin bin) audiokaraoke audioamplify)
                     (check-equal? (gst_element_link_filtered audiokaraoke audioamplify caps) 1))))
   
   
   (test-case
    "Test linking src to dest through their pads using the given caps as filtercaps."
    (with-gst-init #f
                   (let ([bin (gst_bin_new "my-bin")]
                         [audiokaraoke (gst_element_factory_make "audiokaraoke" "audiokaraoke")]
                         [audioamplify (gst_element_factory_make "audioamplify" "audioamplify")]
                         [caps (gst_caps_from_string "audio/x-raw-float")])
                     
                     
                     (gst_bin_add_many (elem-to-bin bin) audiokaraoke audioamplify)
                     (check-equal? (gst_element_link_pads_filtered audiokaraoke "src" audioamplify "sink" caps) 1))))
   
   
   (test-case
    "Test setting and getting element times (base and start) and clocks."
    (with-gst-init #f
                   (let ([autoaudiosink (gst_element_factory_make "autoaudiosink" "autoaudiosink")]
                         [src (gst_element_factory_make "fakesrc" "source")]
                         [pipe (gst_pipeline_new "my-pipe")])
                     
                     ;The base time is the absolute time of the clock when this element was last put to PLAYING. 
                     ;Subtracting the base time from the clock time gives the running time of the element. 
                     (gst_element_set_base_time autoaudiosink 2)
                     (check-equal? (gst_element_get_base_time autoaudiosink) 2)
                     (check-not-equal? (gst_element_get_base_time autoaudiosink) 5)
                     
                     ;The start time of the element is the running time of the element when it last went to the PAUSED state.
                     (gst_element_set_start_time autoaudiosink 6)
                     (check-equal? (gst_element_get_start_time autoaudiosink) 6)
                     (check-not-equal? (gst_element_get_start_time autoaudiosink) 4)
                     
                     (check-equal? (gst_element_provides_clock autoaudiosink) 1) ;;sinks provide a clock, src don't. It depends on the type of plugin.
                     (check-equal? (gst_element_provides_clock src) 0)
                     
                     (gst_element_set_state pipe GST_STATE_PAUSED)
                     (check-equal? (get_Gst_State pipe) GST_STATE_PAUSED) 
                     
                     ;;An element that can provide a clock is only required to do so in the PAUSED
                     (let ([clock (gst_element_provide_clock pipe)])
                       (check-equal? (GstClock-pointer? clock) true)) ;;seems to only work on pipelines
                     )))
   
   
   (test-case
    "Test setting and getting a bus from an element"
    (with-gst-init #f
                   (let ([ogmvideoparse (gst_element_factory_make "ogmvideoparse" "ogmvideoparse")]
                         [bus (gst_bus_new)])
                     
                     (gst_element_set_bus ogmvideoparse bus)
                     
                     (check-equal? (GstBus-pointer? (gst_element_get_bus ogmvideoparse)) true))))
   
   
   (test-case
    "Test posting a message in the element's bus"    
    (with-gst-init #f
                   (let ([vorbisenc (gst_element_factory_make "vorbisenc" "vorbisenc")]
                         [bus (gst_bus_new)])
                     
                     (check-equal? (gst_element_post_message vorbisenc (gst_message_new_warning (GstElement-to-GstObject vorbisenc) (g_error_new (get_GST_CORE_ERROR) GST_CORE_ERROR_TOO_LAZY "dummy error message") "a warning")) 0)
                     
                     (gst_element_set_bus vorbisenc bus)
                     
                     (check-equal? (gst_element_post_message vorbisenc (gst_message_new_warning (GstElement-to-GstObject vorbisenc) (g_error_new (get_GST_CORE_ERROR) GST_CORE_ERROR_TOO_LAZY "dummy error message") "a warning")) 1)
                     
                     (check-equal? (GstMessage-pointer? (gst_bus_pop bus)) true)
                     )))
   
   
   (test-case
    "Test setting and getting an element's index. (not all elements can be indexed!"
    (with-gst-init #f
                   
                   (let ([fakesrc (gst_element_factory_make "fakesrc" "fakesrc")]
                         [mpegparse (gst_element_factory_make "mpegparse" "mpegparse")]
                         [index (gst_index_new)])
                     
                     (check-equal? (gst_element_is_indexable fakesrc) 0)
                     (check-equal? (gst_element_is_indexable mpegparse) 1)
                     (check-equal? (gst_element_get_index mpegparse) #f)
                     (gst_element_set_index mpegparse index)
                     (check-equal? (GstIndex-pointer? (gst_element_get_index mpegparse)) #t) 
                     )))
   
   
   ;;NEED TO DEBUG...THE EVENT IS NOT BEING SEND TO THE PIPE
   (test-case
    "Test sending events to elements"
    (with-gst-init #f
                   (local (
                           (define (event-loop pipe)
                             
                             (let ([bus (gst_element_get_bus pipe)])
                               (let loop ()
                                 (let* ([message (gst_bus_poll bus GST_MESSAGE_ANY -1)]
                                        [type (gst_message_type message)])
                                   (cond
                                     [(eq? type GST_MESSAGE_EOS)
                                      (printf "eos~n")
                                      (gst_message_unref_w message)
                                      #t]
                                     
                                     [else 
                                      (printf "else: ~s~n" type)
                                      (gst_message_unref_w message)
                                      ; (loop)]
                                      ]
                                     ))))))
                     
                     (let ([pipeline (gst_pipeline_new "audio-player")])
                       
                       (gst_element_set_state pipeline GST_STATE_PLAYING)
                       
                       (check-equal? (gst_element_send_event pipeline (gst_event_new_eos)) 1) ;;;THIS IS WHAT IS NOT GETTING SENT
                       
                       (event-loop pipeline)
                       
                       (gst_element_set_state pipeline GST_STATE_NULL)
                       
                       (gst_object_unref pipeline)))))
   
   
   (test-case
    "Test quering an element for information (stream position and duration)"
    (with-gst-init #f
                   (local (
                           (define (buscall bus* msg* data*)
                             (let ([loop (cast data* _gpointer _GMainLoop-pointer)]
                                   [type (gst_message_type msg*)])
                               (cond
                                 [(eq? type GST_MESSAGE_EOS) 
                                  (g_main_loop_quit loop)]
                                 [(eq? type GST_MESSAGE_ERROR)
                                  (printf "Unknown error~n")
                                  (g_main_loop_quit loop)]
                                 [else #f])
                               1))
                           
                           
                           ;;;THE TESTS ARE HERE
                           (define (make_query element*)
                             (let (
                                   ;;[query-type-list (gst_element_get_query_types element*)] TO-DO
                                   [duration-query (gst_query_new_duration GST_FORMAT_TIME)])
                               
                               (check-equal? (GstQuery-pointer? duration-query) #t)
                               
                               (check-equal? (gst_element_query (pointer-to-elem element*) duration-query) 1))
                             
                             (let-values ([(succeed? format position) (gst_element_query_position (pointer-to-elem element*) GST_FORMAT_TIME)])
                               
                               (check-equal? succeed? 1)
                               (check-equal? format GST_FORMAT_TIME)
                               (check-not-equal? position 0))
                             
                             
                             (let-values ([(succeed? format duration) (gst_element_query_duration (pointer-to-elem element*) GST_FORMAT_TIME)])
                               
                               (check-equal? succeed? 1)
                               (check-equal? format GST_FORMAT_TIME)
                               (check-not-equal? duration 0))
                             
                             1))
                     
                     
                     (let* ([pipeline (gst_pipeline_new "audio-player")]
                            (source (gst_element_factory_make "filesrc" "file-source"))
                            (demuxer (gst_element_factory_make "oggdemux" "ogg-demuxer"))
                            (decoder (gst_element_factory_make "vorbisdec" "vorbis-decoder"))
                            (conv (gst_element_factory_make "audioconvert" "converter"))
                            (sink (gst_element_factory_make "autoaudiosink" "audio-output"))
                            [loop (g_main_loop_new #f 0)]
                            [bus (gst_pipeline_get_bus (cast pipeline _GstElement-pointer _GstPipeline-pointer))])
                       
                       (g_object_set_1 source "location" "sample.ogg")
                       
                       (gst_bus_add_watch bus buscall loop)
                       (gst_object_unref bus)
                       
                       (gst_bin_add_many (elem-to-bin pipeline) source demuxer decoder conv sink)
                       
                       (gst_element_link source demuxer)
                       (gst_element_link_many (list decoder conv sink))
                       (signal_connect_pad_added demuxer decoder)
                       
                       (gst_element_set_state pipeline GST_STATE_PLAYING)
                       
                       (check-not-equal? (g_timeout_add 300 make_query pipeline) 0)
                       
                       (g_main_loop_run loop)
                       
                       (gst_element_set_state pipeline GST_STATE_NULL)
                       
                       (gst_object_unref pipeline)))))
   
   
   
   
   
   
   
   
   (test-case
    "Test performing a seek ona given elements"
    (with-gst-init #f
                   
                   (let* ([pipeline (gst_pipeline_new "audio-player")])
                     
                     (gst_element_set_state pipeline GST_STATE_PAUSED)
                     
                     (check-equal? (gst_element_seek_simple  pipeline GST_FORMAT_TIME GST_SEEK_FLAG_FLUSH 0) 1)
                     
                     (gst_object_unref pipeline))))
   
   
   
   
   
   
   
   
   
   
   
   
   ))