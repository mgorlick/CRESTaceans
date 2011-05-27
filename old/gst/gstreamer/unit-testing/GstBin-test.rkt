#lang racket

(require rackunit
         rackunit/text-ui
         "../gstreamer.rkt"
         ffi/unsafe
         srfi/13)

(provide (all-defined-out))

(define GstBin-test
    (test-suite
     "Tests for GstBin library"

     (test-case
      "Test creating a new bin"
      (with-gst-init #f 
                     (begin
                       (check-equal? (GstElement-pointer? (gst_bin_new "my-bin")) #t)
                       (check-not-equal? (GstPad-pointer? (gst_bin_new "my-bin")) #t))))
     
     (test-case
      "Adding and element to a bin"
      (with-gst-init #f 
                     (let ([the-bin (gst_bin_new "my-bin")]
                           [filesrc (gst_element_factory_make "filesrc" "filesrc")])
                       (check-equal? (gst_bin_add (elem-to-bin the-bin) filesrc) 1))))
     
      (test-case
      "Adding many elements to a bin"
      (with-gst-init #f 
                     (let ([the-bin (gst_bin_new "my-bin")]
                           [filesrc (gst_element_factory_make "filesrc" "filesrc")]
                           [sink (gst_element_factory_make "fakesink" "fakesink")])
                       (gst_bin_add_many (elem-to-bin the-bin) filesrc sink)
                       (check-equal? (get_GstBin_numchildren (elem-to-bin the-bin)) 2))))
     
     
     (test-case
      "Removing an element from a bin"
      (with-gst-init #f 
                     (let ([the-bin (gst_bin_new "my-bin")]
                           [filesrc (gst_element_factory_make "filesrc" "filesrc")])
                       (gst_bin_add (elem-to-bin the-bin) filesrc)
                       (check-equal? (get_GstBin_numchildren (elem-to-bin the-bin)) 1)
                       (check-equal? (gst_bin_remove (elem-to-bin the-bin) filesrc) 1)
                       (check-equal? (get_GstBin_numchildren (elem-to-bin the-bin)) 0))))
     
     
      (test-case
      "Removing many elements from a bin"
      (with-gst-init #f 
                     (let ([the-bin (gst_bin_new "my-bin")]
                           [filesrc (gst_element_factory_make "filesrc" "filesrc")]
                           [sink (gst_element_factory_make "fakesink" "fakesink")])
                       (gst_bin_add_many (elem-to-bin the-bin) filesrc sink)
                       (check-equal? (get_GstBin_numchildren (elem-to-bin the-bin)) 2)
                       (gst_bin_remove_many (elem-to-bin the-bin) filesrc sink)
                       (check-equal? (get_GstBin_numchildren (elem-to-bin the-bin)) 0))))
                           
                     
      (test-case
      "Getting an element by name from a bin"
      (with-gst-init #f 
                     (let ([the-bin (gst_bin_new "my-bin")]
                           [sink (gst_element_factory_make "fakesink" "fakesink")])
                       (gst_bin_add (elem-to-bin the-bin) sink)
                       (check-equal? (GstElement-pointer? (gst_bin_get_by_name (elem-to-bin the-bin) "fakesink")) #t)
                       (check-equal? (get_GstBin_numchildren (elem-to-bin the-bin)) 1)))) ;;to check getting the element doesn't mean removing it
      
      
      (test-case
      "Getting an element by name, but performing the search recursing up to the parent bin"
      (with-gst-init #f 
                     (let ([the-bin1 (gst_bin_new "my-bin1")]
                           [sink (gst_element_factory_make "fakesink" "fakesink")]
                           [the-bin2 (gst_bin_new "my-bin2")]
                           [filesrc (gst_element_factory_make "filesrc" "filesrc")])
                       (gst_bin_add (elem-to-bin the-bin1) sink)
                       (gst_bin_add (elem-to-bin the-bin2) filesrc)
                       (gst_bin_add (elem-to-bin the-bin1) the-bin2)
                       (check-equal? (GstElement-pointer? (gst_bin_get_by_name_recurse_up (elem-to-bin the-bin2) "fakesink")) #t)
                       (check-equal? (get_GstElement_name (gst_bin_get_by_name_recurse_up (elem-to-bin the-bin2) "fakesink")) "fakesink"))))
      
      
     #| 
NEEDS TO BIND MANY MACROS TO RETRIEVE INTERFACE GTYPES

(test-case
      "Getting an element by name, but performing the search recursing up to the parent bin"
      (with-gst-init #f
                     (let ([the-bin (gst_bin_new "my-bin")]
                           [filesrc (gst_element_factory_make "filesrc" "filesrc")])
                       (gst_bin_add (elem-to-bin the-bin) filesrc)
                       (check-equal? (gst_bin_get_by_interface (elem-to-bin the-bin) GST_TYPE_URI_HANDLER) filesrc))))|#
      
      (test-case
      "Test the return of an iterator to go through the bin's elements - 
testing gst_bin_iterate_elements, gst_bin_iterate_recurse, gst_bin_iterate_sinks, gst_bin_iterate_sorted and gst_bin_iterate_sources"
      (with-gst-init #f
                     (let* ([the-bin1 (gst_bin_new "my-bin1")]
                            [the-bin2 (gst_bin_new "my-bin2")]
                            
                            [filesrc (gst_element_factory_make "filesrc" "filesrc")]
                            [sink (gst_element_factory_make "fakesink" "fakesink")]
                            [oggmux (gst_element_factory_make "oggmux" "oggmux")]
                            [queue (gst_element_factory_make "queue" "queue")]
                            [filesink (gst_element_factory_make "filesink" "filesink")]
                           
                            [iterator (gst_bin_iterate_recurse (elem-to-bin the-bin1))]
                            [iterator-recurse (gst_bin_iterate_elements (elem-to-bin the-bin2))]
                            [iterator-sinks1 (gst_bin_iterate_sinks (elem-to-bin the-bin1))]
                            [iterator-sinks2 (gst_bin_iterate_sinks (elem-to-bin the-bin2))]
                            [iterator-sorted (gst_bin_iterate_sorted (elem-to-bin the-bin2))]
                            [iterator-sources (gst_bin_iterate_sources (elem-to-bin the-bin1))])
                       
                       (gst_bin_add_many (elem-to-bin the-bin1) filesrc sink the-bin2)
                       (gst_bin_add_many (elem-to-bin the-bin2) oggmux queue filesink)
                       
                       (check-equal? (GstIterator-pointer? iterator) #t)
                       (check-equal? (GstPad-pointer? iterator) #f)
                       
                       (check-equal? (GstIterator-pointer? iterator-recurse) #t)
                       (check-equal? (GstIterator-pointer? iterator-sinks1) #t)
                       (check-equal? (GstIterator-pointer? iterator-sinks2) #t)
                       (check-equal? (GstIterator-pointer? iterator-sorted) #t)
                       (check-equal? (GstIterator-pointer? iterator-sources) #t))))
      
      
      (test-case
      "Test for unlinked pads"
      (with-gst-init #f 
                     (let ([the-bin (gst_bin_new "my-bin")]
                           [fakesrc (gst_element_factory_make "fakesrc" "fakesrc")]
                           [decodebin (gst_element_factory_make "decodebin" "decodebin")]
                           [audioconvert (gst_element_factory_make "audioconvert" "audioconvert")]
                           [audioresample (gst_element_factory_make "audioresample" "audioresample")]
                           [filesink (gst_element_factory_make "esdsink" "esdsink")])
                       
                       (gst_bin_add_many (elem-to-bin the-bin) fakesrc decodebin audioconvert audioresample filesink)
                       (check-equal? (get_GstPad_name (gst_bin_find_unlinked_pad (elem-to-bin the-bin) GST_PAD_SINK)) "sink")
                       (check-equal? (get_GstPad_name (gst_bin_find_unlinked_pad (elem-to-bin the-bin) GST_PAD_SRC)) "src"))))
      
     
       ))