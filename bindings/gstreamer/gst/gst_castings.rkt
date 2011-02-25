#lang racket

(require ffi/unsafe)
(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))


(define (pointer-to-elem element)
  (cast element _pointer _GstElement-pointer))

(define (elem-to-bin element)
  (cast element _GstElement-pointer _GstBin-pointer))

(define (gpointer-to-GstPadTemplatepPointer element)
  (cast element _gpointer _GstPadTemplate-pointer))


(define (GstElement-to-GstElementFactory element)
  (cast element _GstElement-pointer _GstElementFactory-pointer))

(define (GstElement-to-GstObject element)
  (cast element _GstElement-pointer _GstObject-pointer))

(define (elem-to-pipe pipeline)
  (cast pipeline _GstElement-pointer _GstPipeline-pointer))

(define (caps-to-gpointer caps)
  (cast caps _GstCaps-pointer _gpointer))
