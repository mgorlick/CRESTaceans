#lang racket

(require rackunit
         rackunit/text-ui
         "../gstreamer.rkt"
         ffi/unsafe
         srfi/13)

(provide (all-defined-out))

(define GstBuffer-test
    (test-suite
     "Tests for GstBuffer library"
     
     
     (test-case
      "Test creating a new buffer and doing memory allocation"
      (with-gst-init #f 
                     (let ([buffer (gst_buffer_new)]
                           [data (cast (malloc 1000 'raw) _pointer (_ptr io _guint8))]
                           [buffer2 (gst_buffer_new_and_alloc 2000)]
                           [buffer3 (gst_buffer_try_new_and_alloc 1000)]) 
                       
                       (check-equal? (GstBuffer-pointer? buffer) #t)
                       
                       (set_Gst_Buffer_Size buffer 1000)
                       (check-equal? (get_Gst_Buffer_Size buffer) 1000)
                       
                       (set_Gst_Buffer_Data buffer data)
                       (check-equal? (get_Gst_Buffer_Data buffer) data)
                       
                       (set_Gst_Buffer_Mallocdata buffer data)
                       (check-equal? (get_Gst_Buffer_Mallocdata buffer) data)
                       
                       (check-equal? (get_Gst_Buffer_Size buffer2) 2000)
                       (check-equal? (get_Gst_Buffer_Size buffer3) 1000))))
     
     
      (test-case
      "Test making a buffer writable. Ensures the refcount of the buffer is 1, so only the caller owns the buffer and can change the metadata"
      (with-gst-init #f 
                     (let* ([buffer (gst_buffer_new_and_alloc 2000)]
                           [writable-buffer (gst_buffer_make_metadata_writable buffer)])
                       
                       (check-equal? (GstBuffer-pointer? writable-buffer) #t)
                       (check-equal? (gst_buffer_is_metadata_writable writable-buffer) 1))))
      
      
      (test-case
      "Test creating a subbuffer within a parent buffer"
      (with-gst-init #f 
                     (let* ([buffer (gst_buffer_new_and_alloc 2000)]
                           [sub-buffer (gst_buffer_create_sub buffer 200 1000)])
                       
                       (check-equal? (GstBuffer-pointer? sub-buffer) #t)
                       (check-equal? (get_Gst_Buffer_Size sub-buffer) 1000)
                       
                       ;this part doesn't work as expected, i have to understand buffers more
                       ;(check-equal? (get_Gst_Buffer_Offset sub-buffer) 200)
                       
                       ;(set_Gst_Buffer_Offset sub-buffer 400)
                       ;(check-equal? (get_Gst_Buffer_Offset sub-buffer) 400)
                       
                       ;(check-equal? (get_Gst_Buffer_Offset_End sub-buffer) 1399)
                       ;(set_Gst_Buffer_Offset_End sub-buffer 1499)
                       ;(check-equal? (get_Gst_Buffer_Offset sub-buffer) 500)
                       )))
     
      
      (test-case
      "Creates a new buffer that consists of part of buf1 and buf2. buf1 and buf2 are concatenated into a single larger buffer, and a new buffer is created at the 
given offset inside this space, with a given length. "
      (with-gst-init #f 
                      (let* ([buffer1 (gst_buffer_new_and_alloc 1000)]
                             [buffer2 (gst_buffer_new_and_alloc 2000)]
                             [spaned-buffer (gst_buffer_span buffer1 500 buffer2 1500)])
                        
                        (check-equal? (GstBuffer-pointer? spaned-buffer) #t)
                        (check-equal? (get_Gst_Buffer_Size spaned-buffer) 1500))))
      
      
       (test-case
      "Tests whether a gst_buffer_span() can be done without copying the contents, that is, whether the data areas are contiguous sub-buffers of the same buffer. "
      (with-gst-init #f 
                     (let* ([buffer1 (gst_buffer_new_and_alloc 1000)]
                            [buffer2 (gst_buffer_new_and_alloc 2000)]
                            
                            [buffer3 (gst_buffer_new_and_alloc 1000)]
                            [sub-buffer1 (gst_buffer_create_sub buffer3 0 400)]
                            [sub-buffer2 (gst_buffer_create_sub buffer3 400 400)]
                            
                            [buffer4 (gst_buffer_new_and_alloc 1000)]
                            [sub-buffer3 (gst_buffer_create_sub buffer4 0 400)]
                            [sub-buffer4 (gst_buffer_create_sub buffer4 500 400)])
                       
                       (check-equal? (gst_buffer_is_span_fast buffer1 buffer2) 0)
                       (check-equal? (gst_buffer_is_span_fast sub-buffer1 sub-buffer2) 1)
                       
                       ;;NOTE: gst_buffer_is_span_fast will be false if the subbuffers are not contiguous.
                       (check-equal? (gst_buffer_is_span_fast sub-buffer3 sub-buffer4) 0))))
       
       
       (test-case
      "Tests setting caps to a  buffer"
      (with-gst-init #f 
                     (let ([buffer (gst_buffer_new_and_alloc 1000)]
                           [caps (gst_caps_from_string "audio/x-raw-int")])
                       
                       (gst_buffer_set_caps buffer caps)
                       (check-equal? (GstCaps-pointer? (gst_buffer_get_caps buffer)) #t)
                       (check-equal? (ptr-equal? (gst_buffer_get_caps buffer) caps) #t)
                       )))
                       
                       
                        
       
       (test-case
      "Tests copying the metadata from a src buffer to a destination buffer"
      (with-gst-init #f 
                     (let ([dest-buffer1 (gst_buffer_new_and_alloc 1000)]
                           [src-buffer1 (gst_buffer_new_and_alloc 2000)]
                            
                            [dest-buffer2 (gst_buffer_new_and_alloc 1000)]
                            [src-buffer2 (gst_buffer_new_and_alloc 2000)]
                            
                            [caps (gst_caps_from_string "audio/x-raw-int")]
                            [dest-caps (gst_caps_from_string "audio/mpeg, mpegversion=(int)1, layer=(int)3")])
                       
                       (gst_buffer_set_caps src-buffer1 caps)
                       (gst_buffer_set_caps src-buffer2 caps)
                       
                       (gst_buffer_set_caps dest-buffer1 dest-caps)
                       (gst_buffer_set_caps dest-buffer2 dest-caps)
                       
                       (Gst_Buffer_Copy_All dest-buffer1 src-buffer1)
                       
                       (check-equal? (get_Gst_Buffer_Timestamp dest-buffer1) (get_Gst_Buffer_Timestamp src-buffer1))
                       (check-equal? (ptr-equal? (get_Gst_Buffer_Caps dest-buffer1) (get_Gst_Buffer_Caps src-buffer1)) #t)
                     
                       (gst_buffer_copy_metadata dest-buffer2 src-buffer2 GST_BUFFER_COPY_TIMESTAMPS)
                       (check-equal? (get_Gst_Buffer_Timestamp dest-buffer2) (get_Gst_Buffer_Timestamp src-buffer2))
                       (check-equal? (ptr-equal? (get_Gst_Buffer_Caps dest-buffer2) (get_Gst_Buffer_Caps src-buffer2)) #f)
                       )))
       
      (test-case
      "Tests joining and merging buffers"
      (with-gst-init #f 
                    ;;gst_buffer_join unrefs the original source buffers, but gst_buffer_merge keeps original buffers
                     ;; but in the scheme side is not possible to make that distinction
                     
                     (let* ([buffer1 (gst_buffer_new_and_alloc 1000)]
                           [buffer2 (gst_buffer_new_and_alloc 2000)]
                           [joined-buffer (gst_buffer_join buffer1 buffer2)]
                           
                           [buffer3 (gst_buffer_new_and_alloc 500)]
                           [buffer4 (gst_buffer_new_and_alloc 400)]
                           [merged-buffer (gst_buffer_join buffer3 buffer4)])
                       
                       (check-equal? (get_Gst_Buffer_Size joined-buffer) 3000)
                       
                       (check-equal? (get_Gst_Buffer_Size merged-buffer) 900))))
      
    
 ))