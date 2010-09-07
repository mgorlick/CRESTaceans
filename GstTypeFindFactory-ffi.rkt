#lang racket

(require "gst_base.rkt"
         "GstCaps-ffi.rkt"
         "GstTypeFind-ffi.rkt")

(provide (all-defined-out))


;;typedef struct _GstTypeFindFactory GstTypeFindFactory;
(define-cpointer-type _GstTypeFindFactory-pointer)

;GList *             gst_type_find_factory_get_list      (void);
(define-gstreamer gst_type_find_factory_get_list (_fun -> _GList-pointer))

;gchar **            gst_type_find_factory_get_extensions (GstTypeFindFactory *factory);
(define-gstreamer gst_type_find_factory_get_extensions (_fun _GstTypeFindFactory-pointer -> (_ptr io _string)))

;GstCaps *           gst_type_find_factory_get_caps      (GstTypeFindFactory *factory);
(define-gstreamer gst_type_find_factory_get_caps (_fun _GstTypeFindFactory-pointer -> (_ptr io _GstCaps)))

;void                gst_type_find_factory_call_function (GstTypeFindFactory *factory, GstTypeFind *find);
(define-gstreamer gst_type_find_factory_call_function (_fun _GstTypeFindFactory-pointer _GstTypeFind-pointer -> _void))
