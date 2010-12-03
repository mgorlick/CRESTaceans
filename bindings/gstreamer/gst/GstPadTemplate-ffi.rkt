#lang racket

(require "gst_base.rkt"
         "gst-structs-ffi.rkt"
         "GstObject-ffi.rkt")

(provide (all-defined-out))



#|typedef enum {
  GST_PAD_TEMPLATE_FIXED        = (GST_OBJECT_FLAG_LAST << 0),
  /* padding */
  GST_PAD_TEMPLATE_FLAG_LAST    = (GST_OBJECT_FLAG_LAST << 4)
} GstPadTemplateFlags;|#

(define GST_PAD_TEMPLATE_FIXED (arithmetic-shift GST_OBJECT_FLAG_LAST 0))
(define GST_PAD_TEMPLATE_FLAG_LAST (arithmetic-shift GST_OBJECT_FLAG_LAST 4))

  
#|typedef enum {
  GST_PAD_ALWAYS,
  GST_PAD_SOMETIMES,
  GST_PAD_REQUEST
} GstPadPresence;|#

(define GST_PAD_ALWAYS 0)
(define GST_PAD_SOMETIMES 1)
(define GST_PAD_REQUEST 2)


#|#define             GST_STATIC_PAD_TEMPLATE             (padname, dir, pres, caps)
#define             GST_PAD_TEMPLATE_NAME_TEMPLATE      (templ)
#define             GST_PAD_TEMPLATE_DIRECTION          (templ)
#define             GST_PAD_TEMPLATE_PRESENCE           (templ)
#define             GST_PAD_TEMPLATE_CAPS               (templ)
#define             GST_PAD_TEMPLATE_IS_FIXED           (templ)|#


;GstPadTemplate *    gst_static_pad_template_get         (GstStaticPadTemplate *pad_template);
(define-gstreamer gst_static_pad_template_get (_fun _GstStaticPadTemplate-pointer -> _GstPadTemplate-pointer))

;GstCaps*            gst_static_pad_template_get_caps    (GstStaticPadTemplate *templ);
(define-gstreamer gst_static_pad_template_get_caps (_fun _GstStaticPadTemplate-pointer -> _GstCaps-pointer))


;GstPadTemplate*     gst_pad_template_new                (const gchar *name_template, GstPadDirection direction, GstPadPresence presence, GstCaps *caps);
(define-gstreamer gst_pad_template_new (_fun _string _int _int _GstCaps-pointer -> _GstPadTemplate-pointer))

;GstCaps*            gst_pad_template_get_caps           (GstPadTemplate *templ);
(define-gstreamer gst_pad_template_get_caps (_fun _GstPadTemplate-pointer -> _GstCaps-pointer))
