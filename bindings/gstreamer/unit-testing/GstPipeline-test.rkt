#lang racket

(require rackunit
         rackunit/text-ui
         "../gstreamer.rkt"
         ffi/unsafe
         srfi/13)

(provide (all-defined-out))


(define GstPipeline-test
  (test-suite
   "Tests for GstPipeline library"
   
   
   (test-case
    "Test creating a new pipeline."
    (with-gst-init #f
                   
                   (let ([pipe (gst_pipeline_new "my-pipe")])
                     
                     (check-equal? (GstElement-pointer? pipe) #t)
                     (check-equal? (get_gst_element_get_name pipe) "my-pipe"))))
   
   
   (test-case
    "Test getting the bus from a pipeline."
    (with-gst-init #f
                   
                   (let* ([pipe (gst_pipeline_new "my-pipe")]
                          [bus (gst_pipeline_get_bus (elem-to-pipe pipe))])
                     
                     (check-equal? (GstBus-pointer? bus) #t))))
   
   
   (test-case
    "Test setting and getting the clock for a pipeline."
    (with-gst-init #f
                   
                   (let* ([pipe (gst_pipeline_new "my-pipe")]
                          [pipe2 (gst_pipeline_new "my-pipe2")]
                          [clock (gst_pipeline_get_clock (elem-to-pipe pipe))])
                     
                     (check-equal? (GstClock-pointer? clock)  #t)
                     (check-equal? (gst_pipeline_set_clock (elem-to-pipe pipe2) clock)  1)
                     (gst_pipeline_use_clock (elem-to-pipe pipe2) clock)
                     (gst_pipeline_auto_clock (elem-to-pipe pipe2)))))
   
   
   (test-case
    "Test setting and checking whether the pipline's bus is in autoflush mode (automatically flushes all pending messages on the bus, which is done for refcounting purposes)"
    (with-gst-init #f
                   
                   (let ([pipe (gst_pipeline_new "my-pipe")])
                     (check-equal? (gst_pipeline_get_auto_flush_bus (elem-to-pipe pipe)) 1)
                     (gst_pipeline_set_auto_flush_bus (elem-to-pipe pipe) 0)
                     (check-equal? (gst_pipeline_get_auto_flush_bus (elem-to-pipe pipe)) 0))))
   
   
   (test-case
    "Test setting the expected delay needed for all elements to perform the PAUSED to PLAYING state change"
    (with-gst-init #f
                   
                    (let ([pipe (gst_pipeline_new "my-pipe")])
                      
                      (gst_pipeline_set_delay (elem-to-pipe pipe) 10)
                      
                      (check-equal? (gst_pipeline_get_delay (elem-to-pipe pipe)) 10))))
                      
                         
   ))

