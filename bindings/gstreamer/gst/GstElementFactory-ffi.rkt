#lang racket

(require "gst_base.rkt"
         "GstStructs-ffi.rkt"
         "GstCaps-ffi.rkt")

(provide (all-defined-out))


;#define             GST_ELEMENT_DETAILS                 (longname, klass, description, author)
;#define             GST_IS_ELEMENT_DETAILS              (details)


;GstElementFactory * gst_element_factory_find            (const gchar *name);
;(define-gstreamer gst_element_factory_find (_fun _string -> (_or-null _GstElementFactory-pointer)))
(define-gstreamer gst_element_factory_find (_fun _string -> _GstElementFactory-pointer))

;GType               gst_element_factory_get_element_type (GstElementFactory *factory);
(define-gstreamer gst_element_factory_get_element_type (_fun _GstElementFactory-pointer -> _GType))        

;GstElementFactory* -> gchar*
(define-gstreamer*
  (_fun _GstElementFactory-pointer -> _string)
  gst_element_factory_get_longname gst_element_factory_get_klass gst_element_factory_get_description gst_element_factory_get_author)
                                                        
;guint               gst_element_factory_get_num_pad_templates (GstElementFactory *factory);
(define-gstreamer gst_element_factory_get_num_pad_templates (_fun _GstElementFactory-pointer -> _guint)) 

;gint                gst_element_factory_get_uri_type    (GstElementFactory *factory);
(define-gstreamer gst_element_factory_get_uri_type (_fun _GstElementFactory-pointer -> _gint))

;gchar **            gst_element_factory_get_uri_protocols (GstElementFactory *factory);
(define-gstreamer gst_element_factory_get_uri_protocols (_fun _GstElementFactory-pointer -> (_ptr o _string)))

;gboolean            gst_element_factory_has_interface   (GstElementFactory *factory, const gchar *interfacename);
(define-gstreamer gst_element_factory_has_interface (_fun _GstElementFactory-pointer _string -> _gboolean))

;GstElement*         gst_element_factory_create          (GstElementFactory *factory, const gchar *name);
(define-gstreamer gst_element_factory_create (_fun _GstElementFactory-pointer _string -> _GstElement-pointer))

;GstElement*         gst_element_factory_make            (const gchar *factoryname, const gchar *name);
(define-gstreamer gst_element_factory_make (_fun _string _string -> _GstElement-pointer))

;GstElementFactory* GstCaps* -> gboolean
(define-gstreamer*
  (_fun _GstElementFactory-pointer _GstCaps-pointer -> _gboolean)
  gst_element_factory_can_sink_caps gst_element_factory_can_src_caps)

;const GList *       gst_element_factory_get_static_pad_templates (GstElementFactory *factory);
(define-gstreamer gst_element_factory_get_static_pad_templates (_fun _GstElementFactory-pointer -> _GList-pointer))

