#! /usr/bin/env racket
#lang racket

(require "../gst/gstreamer-ffi.rkt")

(gst_init #f #f)


(define (make name)
  (gst_element_factory_create (gst_element_factory_find name) "name"))

(let ((pipeline (make "autoaudiosink")))
  (display (gst_element_factory_get_longname (gst_element_factory_find "autoaudiosink"))))



