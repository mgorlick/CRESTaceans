#lang racket

(require rackunit
         rackunit/text-ui
         "gst-test.rkt"
         "GstBin-test.rkt"
         "GstBuffer-test.rkt"
         "GstElementFactory-test.rkt"
         "GstElement-test.rkt"
         "GstPipeline-test.rkt"
         "GstCaps-test.rkt")


(run-tests gst-initialization-test)

(run-tests GstElementFactory-test)

(run-tests GstElement-test)

(run-tests GstBin-test)

(run-tests GstBuffer-test)

(run-tests GstCaps-test)

;(run-tests GstIterator-test)

(run-tests GstPipeline-test)



