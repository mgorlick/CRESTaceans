#lang racket

(require rackunit
         rackunit/text-ui
         "../gstreamer.rkt"
         ffi/unsafe
         srfi/13)

(provide (all-defined-out))

#|(define GstIterator-test
  (test-suite
   "Tests for GstIterator library"
   
   
   
   (test-case
    "Test setting and getting an element's name."
    (with-gst-init #f|#