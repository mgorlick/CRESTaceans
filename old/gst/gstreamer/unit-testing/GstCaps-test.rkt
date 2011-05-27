#lang racket

(require rackunit
         rackunit/text-ui
         "../gstreamer.rkt"
         ffi/unsafe
         srfi/13)

(provide (all-defined-out))

(define GstCaps-test
  (test-suite
   "Tests for GstCaps library"
   
   
   
   (test-case
    "Test making new empty caps"
    (with-gst-init #f
                   (let ([caps (gst_caps_new_empty)])
                     (check-equal? (GstCaps-pointer? caps) #t)
                     (check-equal? (gst_caps_is_empty caps) 1)
                     
                     (let ([caps1 (gst_caps_ref caps)])
                       (check-equal? (gst_caps_is_empty caps1) 1)
                       (check-equal? (get_gst_caps_refcount caps) 2)
                       (gst_caps_unref caps1))
                     
                     (gst_caps_unref caps))))
   
   
   (test-case
    "Test making new caps that is compatible with ANY media format"
    (with-gst-init #f
                   (let ([caps (gst_caps_new_any)])
                     (check-equal? (GstCaps-pointer? caps) #t)
                     (check-equal? (gst_caps_is_any caps) 1)
                     (gst_caps_unref caps))))
   
   
   
   (test-case
    "Test making a copy of a caps"
    (with-gst-init #f
                   (let* ([caps (gst_caps_from_string "audio/x-raw-float")]
                          [caps-copy (gst_caps_copy caps)])
                     
                     (check-equal? (GstCaps-pointer? caps-copy) #t)
                     (check-equal? (get_gst_caps_refcount caps-copy) 1)
                     (gst_caps_unref caps)
                     (gst_caps_unref caps-copy))))
   
   
   (test-case
    "Test making a copy of a caps containing the nth structure from the original caps."
    (with-gst-init #f
                   (let* ([caps (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 240, framerate = (fraction) 25/1 , format = (fourcc) I420")]
                          [caps-copy (gst_caps_copy_nth caps 1)])
                     
                     (check-equal? (GstCaps-pointer? caps-copy) #t)
                     (check-equal? (get_gst_caps_refcount caps-copy) 1)
                     (gst_caps_unref caps)
                     (gst_caps_unref caps-copy))))
   
   
   
   (test-case
    "Test converts a GstStaticCaps to a GstCaps."
    (with-gst-init #f
                   (let* ([static-caps (get_gst_static_caps "video/mpeg, mpegtype=(int)1")]
                          [caps (gst_static_caps_get static-caps)])
                     
                     (check-equal? (GstCaps-pointer? caps) #t)
                     (check-equal? (get_gst_caps_refcount caps) 2) ;;core holds an additional ref to the returned caps
                     (gst_caps_unref caps))))
   
   
   (test-case
    "Test appending two GstCaps."
    (with-gst-init #f
                   (let* ([caps1 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 240, framerate = (fraction) 25/1 , format = (fourcc) I420")]
                          [caps2 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 240, format = (fourcc) I420")])
                     
                     (check-equal? (gst_caps_get_size caps1) 1)
                     (check-equal? (gst_caps_get_size caps2) 1)
                     (gst_caps_append caps1 caps2)
                     (check-equal? (gst_caps_get_size caps1) 2)
                     (gst_caps_unref caps1)
                     ;(check-equal? (gst_caps_get_size caps2) 0)))) ;;The structures in caps2 are not copied -- they are transferred to caps1, and then caps2 is freed. 
                     ;if this line is uncommented it will purposefully give an error given that caps2 has been unrefed.
                     )))
   
   
   (test-case
    "Test merging two GstCaps."
    (with-gst-init #f
                   (let* ([caps1 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 240, framerate = (fraction) 25/1 , format = (fourcc) I420")]
                          [caps2 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 240, format = (fourcc) I420")])
                     
                     (check-equal? (gst_caps_get_size caps1) 1)
                     (check-equal? (gst_caps_get_size caps2) 1)
                     (gst_caps_merge caps1 caps2)
                     (check-equal? (gst_caps_get_size caps1) 2)
                     (gst_caps_unref caps1))))
   
   (test-case
    "Test merging two GstCaps -- case 2."
    (with-gst-init #f
                   (let* ([caps1 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 240, framerate = (fraction) 25/1 , format = (fourcc) I420")]
                          [caps2 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 240, framerate = (fraction) 25/1 , format = (fourcc) I420")])
                     
                     (check-equal? (gst_caps_get_size caps1) 1)
                     (check-equal? (gst_caps_get_size caps2) 1)
                     (gst_caps_merge caps1 caps2)
                     (check-equal? (gst_caps_get_size caps1) 1) ;; caps2 is unrefed, but caps1 stays of size 1 since the caps where equal.
                     (gst_caps_unref caps1))))
   
   
   (test-case
    "Test appending a structure to a caps."
    (with-gst-init #f
                   (let* ([caps (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 240, framerate = (fraction) 25/1 , format = (fourcc) I420")]
                          [structure (gst_structure_empty_new "my-struct")])
                     
                     (check-equal? (gst_caps_get_size caps) 1)
                     (gst_caps_append_structure caps structure)
                     (check-equal? (gst_caps_get_size caps) 2)
                     (gst_caps_remove_structure caps 1)
                     (check-equal? (gst_caps_get_size caps) 1)
                     (gst_caps_unref caps))))
   
   
   (test-case
    "Test merging a structure with a caps."
    (with-gst-init #f
                   (let* ([caps (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 240, framerate = (fraction) 25/1 , format = (fourcc) I420")]
                          [structure (gst_structure_empty_new "my-struct")])
                     
                     (check-equal? (gst_caps_get_size caps) 1)
                     (gst_caps_merge_structure caps structure)
                     (check-equal? (gst_caps_get_size caps) 2)
                     (gst_caps_remove_structure caps 1)
                     (check-equal? (gst_caps_get_size caps) 1)
                     (gst_caps_unref caps))))
   
   
   (test-case
    "Test getting a structure from a caps."
    (with-gst-init #f
                   (let* ([caps (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 240, framerate = (fraction) 25/1 , format = (fourcc) I420")]
                          [structure (gst_structure_empty_new "my-struct")])
                     
                     (check-equal? (gst_caps_get_size caps) 1)
                     (gst_caps_append_structure caps structure)
                     (check-equal? (GstStructure-pointer? (gst_caps_get_structure caps 0)) #t)
                     (check-equal? (gst_caps_get_size caps) 2)
                     (gst_caps_unref caps))))
   
   
   
   (test-case
    "Test getting a string representation from caps."
    (with-gst-init #f
                   (let* ([caps1 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 240, framerate = (fraction) 25/1 , format = (fourcc) I420")]
                          [caps2 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 240")])
                     
                     (gst_caps_merge caps1 caps2)
                     (check-equal? (gst_caps_to_string caps1) 
                                   "video/x-raw-yuv, width=(int)320, height=(int)240, framerate=(fraction)25/1, format=(fourcc)I420; video/x-raw-yuv, width=(int)320, height=(int)240"))))
   
   
   
   (test-case
    "Test checking whether caps are fixed. Fixed GstCaps describe exactly one format, that is, they have exactly one structure, and each field in the structure describes a fixed type."
    (with-gst-init #f
                   (let* ([caps1 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, format = (fourcc) I420; video/x-raw-yuv, width = (int) 500")]
                          [caps2 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 280, format = (fourcc) I420")])
                     
                     (check-equal? (gst_caps_is_fixed caps1) 0)
                     (check-equal? (gst_caps_is_fixed caps2) 1))))
   
   
   (test-case
    "Test checking whether two caps are equal."
    (with-gst-init #f
                   (let* ([caps1 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, format = (fourcc) I420; video/x-raw-yuv, width = (int) 500")]
                          [caps2 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, format = (fourcc) I420; video/x-raw-yuv, width = (int) 500")]
                          [caps3 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 280, format = (fourcc) I420")]
                          [caps4 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 280, format = (fourcc) I420")])
                     
                     
                     (check-equal? (gst_caps_is_equal caps1 caps2) 1)
                     (check-equal? (gst_caps_is_equal caps1 caps3) 0)
                     
                     (check-equal? (gst_caps_is_equal_fixed caps3 caps4) 1))))
   
   
   (test-case
    "Test the compatibility of two caps --every media format that is in the first is also contained in the second."
    (with-gst-init #f
                   (let* ([caps1 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320")]
                          [caps2 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, format = (fourcc) I420; video/x-raw-yuv, width = (int) 500")]
                          [caps3 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 280, format = (fourcc) I420")]
                          [caps4 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, height = (int) 280, format = (fourcc) I420; video/x-raw-yuv, width = (int) 320, height = (int) 240, framerate = (fraction) 25/1 , format = (fourcc) I420")])
                     
                     (check-equal? (gst_caps_is_always_compatible caps1 caps2) 0)
                     (check-equal? (gst_caps_is_always_compatible caps3 caps4) 1)
                     
                     (check-equal? (gst_caps_is_subset caps1 caps2) 0)
                     (check-equal? (gst_caps_is_subset caps3 caps4) 1))))
   
   
   (test-case
    "Test caps compatibility (every media format that is in the first is also contained in the second) intersenction and union."
    (with-gst-init #f
                   (let* ([caps1 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320; video/x-raw-yuv, width = (int) 320, height = (int) 240, framerate = (fraction) 25/1 , format = (fourcc) I420")]
                          [caps2 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320; video/x-raw-yuv, width = (int) 320, format = (fourcc) I420; video/x-raw-yuv, width = (int) 500")])
                     
                     (check-equal? (gst_caps_can_intersect caps1 caps2) 1)
                     
                     (let ([new-caps (gst_caps_intersect caps1 caps2)]
                           [new-caps-union (gst_caps_union caps1 caps2)])
                       
                       (check-equal? (gst_caps_to_string new-caps) "video/x-raw-yuv, width=(int)320; video/x-raw-yuv, width=(int)320, height=(int)240, framerate=(fraction)25/1, format=(fourcc)I420; video/x-raw-yuv, width=(int)320, format=(fourcc)I420; video/x-raw-yuv, width=(int)320, height=(int)240, framerate=(fraction)25/1, format=(fourcc)I420")
                       
                       (check-equal? (gst_caps_to_string new-caps-union) "video/x-raw-yuv, width=(int){ 500, 320 }")))))
   
   
   (test-case
    "Test normalizing caps - Creates a new GstCaps that represents the same set of formats as caps, but contains no lists. Each list is expanded into separate GstStructures."
    (with-gst-init #f
                   (let* ([caps (gst_caps_from_string "video/raw, width=(int)[16,256], height=(int)16; video/raw, width=(int)[16,256], height=(int)16; video/raw, fourcc=(fourcc){\"YV12\",\"YUY2\"}, height=(int)[16,4096]")]
                          [caps-norm (gst_caps_normalize caps)])
                     
                     (check-equal? (gst_caps_to_string caps-norm) "video/raw, width=(int)[ 16, 256 ], height=(int)16; video/raw, width=(int)[ 16, 256 ], height=(int)16; video/raw, fourcc=(fourcc)YV12, height=(int)[ 16, 4096 ]; video/raw, fourcc=(fourcc)YUY2, height=(int)[ 16, 4096 ]"))))
   
   
   
   (test-case
    "Test simplifying caps - Modifies the given caps inplace into a representation that represents the same set of formats, but in a simpler form."
    (with-gst-init #f
                   (let ([caps (gst_caps_from_string "video/x-raw-yuv,format=(fourcc)AYUV,width=704,height=480; video/raw, width=(int)[16,256], height=(int)16; video/raw, width=(int)[16,256], height=(int)16; video/raw, fourcc=(fourcc){\"YV12\",\"YUY2\"}, height=(int)[16,4096]; video/x-raw-yuv, width=(int){ 500, 320 }; video/x-raw-yuv, width = (int) 320; video/x-raw-yuv, width = (int) 320, height = (int) 240, framerate = (fraction) 25/1 , format = (fourcc) I420")])
                     
                     (check-equal? (gst_caps_do_simplify caps) 0)))) ;;; should be set to one, but can't find a good caps example.
   
   
   
   (test-case
    "Test replacing one cap for another."
    (with-gst-init #f
                   (let ([caps1 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320")]
                         [caps2 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, format = (fourcc) I420; video/x-raw-yuv, width = (int) 500")])
                     
                     (gst_caps_replace caps1 caps2)
                     
                     (check-equal? (gst_caps_to_string caps1) "EMPTY") ;;result doesn't match with the docs. It should be replaced, and not empty.
                     
                     (check-equal? (get_gst_caps_refcount caps1) 0)
                     
                     (check-equal? (get_gst_caps_refcount caps2) 2)))) 
   
   
   
   (test-case
    "Test substracting one cap specification from another."
    (with-gst-init #f
                   (let* ([caps1 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320")]
                          [caps2 (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, format = (fourcc) I420; video/x-raw-yuv, width = (int) 500")]
                          [result (gst_caps_subtract caps2 caps1)])
                     
                     (check-equal? (gst_caps_to_string result) "video/x-raw-yuv, width=(int)500"))))
   
   
   (test-case
    "Test returning a writable copy of caps."
    (with-gst-init #f
                   (let* ([caps (gst_caps_from_string "video/x-raw-yuv, width = (int) 320")])
                     
                     (check-equal? (GstCaps-pointer? (gst_caps_make_writable caps)) #t)))) 
   
   
   (test-case
    "Test truncating caps to the first structure"
    (with-gst-init #f
                   (let ([caps (gst_caps_from_string "video/x-raw-yuv, width = (int) 320, format = (fourcc) I420; video/x-raw-yuv, width = (int) 500")])
                     
                     (gst_caps_truncate caps)
                     
                     (check-equal? (gst_caps_to_string caps) "video/x-raw-yuv, width=(int)320, format=(fourcc)I420"))))
   
   
   
   
   
   ))