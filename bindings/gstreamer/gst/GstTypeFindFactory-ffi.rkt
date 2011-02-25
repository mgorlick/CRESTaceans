#lang racket

(require ffi/unsafe)
(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))


;GList *             gst_type_find_factory_get_list      (void);
(define-gstreamer gst_type_find_factory_get_list (_fun -> _GList-pointer))

;gchar **            gst_type_find_factory_get_extensions (GstTypeFindFactory *factory);
(define-gstreamer gst_type_find_factory_get_extensions (_fun _GstTypeFindFactory-pointer -> (_ptr io _string)))

;GstCaps *           gst_type_find_factory_get_caps      (GstTypeFindFactory *factory);
(define-gstreamer gst_type_find_factory_get_caps (_fun _GstTypeFindFactory-pointer -> _GstCaps-pointer))

;void                gst_type_find_factory_call_function (GstTypeFindFactory *factory, GstTypeFind *find);
(define-gstreamer gst_type_find_factory_call_function (_fun _GstTypeFindFactory-pointer _GstTypeFind-pointer -> _void))
