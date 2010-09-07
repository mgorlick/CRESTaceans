#lang racket


;;adapted from the GStreamer bindings for Guile.


(require "gstreamer-ffi.rkt")

(provide (all-defined-out) (all-from-out "gstreamer-ffi.rkt"))

(define (add bin . args)
  (for-each (lambda (element) (gst_bin_add bin element)) args))

(define (remove bin . args)
  (for-each (lambda (element) (gst_bin_remove bin element)) args))

(define (link e1 e2 . rest)
  (if (gst_element_link e1 e2)
      (if (not (null? rest))
          (apply link (cons e2 rest))
          #t)
      #f))

(define (unlink e1 e2 . rest)
  (if (gst_element_unlink e1 e2)
      (if (not (null? rest))
          (apply unlink (cons e2 rest))
          #t)
      #f))


(define scheme-elements (make-hash))

(define (make name)
  (let ((factory (gst_element_factory_find name)))
    (cond
     (factory
      (gst_element_factory_create
       (gst_plugin_feature_load factory) name))
     ((hash-ref scheme-elements name #f)
      (let ((element (make (hash-ref scheme-elements name #f))))
        ;(set-name element #f) don't know to what module set-name belongs
        element))
     (else
      (error "Could not find an element factory named ~A" name)))))


(define (get-pad-template-list class)
  (gst_element_class_get_pad_template_list class))


