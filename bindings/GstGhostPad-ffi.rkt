#lang racket

(require "gst_base.rkt"
         "GstStructs-ffi.rkt"
         "GstPadTemplate-ffi.rkt")

(provide (all-defined-out))

;;typedef struct _GstGhostPad GstGhostPad;
(define-cpointer-type _GstGhostPad-pointer)


;GstPad*             gst_ghost_pad_new                   (const gchar *name, GstPad *target);
(define-gstreamer gst_ghost_pad_new (_fun _string _GstPad-pointer -> _GstPad-pointer))

;GstPad*             gst_ghost_pad_new_no_target         (const gchar *name, GstPadDirection dir);
(define-gstreamer gst_ghost_pad_new_no_target (_fun _string _GstPadDirection -> _GstPad-pointer))

;GstPad*             gst_ghost_pad_new_from_template     (const gchar *name, GstPad *target, GstPadTemplate *templ);
(define-gstreamer gst_ghost_pad_new_from_template (_fun _string _GstPad-pointer _GstPadTemplate-pointer -> _GstPad-pointer))

;GstPad*             gst_ghost_pad_new_no_target_from_template (const gchar *name, GstPadTemplate *templ);
(define-gstreamer gst_ghost_pad_new_no_target_from_template (_fun _string _GstPadTemplate-pointer -> _GstPad-pointer))

;gboolean            gst_ghost_pad_set_target            (GstGhostPad *gpad, GstPad *newtarget);
(define-gstreamer gst_ghost_pad_set_target (_fun _GstGhostPad-pointer _GstPad-pointer -> _gboolean))

;GstPad*             gst_ghost_pad_get_target            (GstGhostPad *gpad);
(define-gstreamer gst_ghost_pad_get_target (_fun _GstGhostPad-pointer -> _GstPad-pointer))

;gboolean            gst_ghost_pad_construct             (GstGhostPad *gpad);
(define-gstreamer gst_ghost_pad_construct (_fun _GstGhostPad-pointer -> _gboolean))
