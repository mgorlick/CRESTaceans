#lang racket

(require ffi/unsafe)
(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))



#|typedef enum
{
  GST_OBJECT_DISPOSING = (1<<0),
  GST_OBJECT_FLOATING = (1<<1),
  /* padding */
  GST_OBJECT_FLAG_LAST = (1<<4)
} GstObjectFlags;|#

(define GST_OBJECT_DISPOSING (arithmetic-shift 1 0))
(define GST_OBJECT_FLOATING (arithmetic-shift 1 1))
(define GST_OBJECT_FLAG_LAST (arithmetic-shift 1 4))


;#define             GST_OBJECT_FLAGS                    (obj)
;#define             GST_OBJECT_FLAG_IS_SET              (obj, flag)
;#define             GST_OBJECT_FLAG_SET                 (obj, flag)
;#define             GST_OBJECT_FLAG_UNSET               (obj, flag)
;#define             GST_OBJECT_NAME                     (obj)
;#define             GST_OBJECT_PARENT                   (obj)
;#define             GST_OBJECT_IS_DISPOSING             (obj)
;#define             GST_OBJECT_IS_FLOATING              (obj)
;#define             GST_OBJECT_REFCOUNT                 (obj)
;#define             GST_OBJECT_REFCOUNT_VALUE           (obj)
;#define             GST_CLASS_GET_LOCK                  (obj)
;#define             GST_CLASS_LOCK                      (obj)
;#define             GST_CLASS_TRYLOCK                   (obj)
;#define             GST_CLASS_UNLOCK                    (obj)
;#define             GST_OBJECT_LOCK                     (obj)
;#define             GST_OBJECT_TRYLOCK                  (obj)
;#define             GST_OBJECT_UNLOCK                   (obj)
;#define             GST_OBJECT_GET_LOCK                 (obj)

;gboolean            gst_object_set_name                 (GstObject *object, const gchar *name);
(define-gstreamer gst_object_set_name (_fun _GstObject-pointer _string -> _gboolean))

;;GstObject*-> gchar*
(define-gstreamer*
  (_fun _GstObject-pointer -> _string)
  gst_object_get_name gst_object_get_path_string)

;;GstObject* GstObject* -> gboolean
(define-gstreamer*
  (_fun _GstObject-pointer _GstObject-pointer -> _gboolean)
  gst_object_set_parent gst_object_has_ancestor)

;;GstObject* -> void
(define-gstreamer gst_object_unparent (_fun _GstObject-pointer -> _void))

;GstObject*          gst_object_get_parent               (GstObject *object);
(define-gstreamer gst_object_get_parent (_fun _GstObject-pointer -> _void))

;void                gst_object_default_deep_notify      (GObject *object, GstObject *orig, GParamSpec *pspec, gchar **excluded_props);
(define-gstreamer gst_object_default_deep_notify (_fun _GstObject-pointer _GstObject-pointer _GParamSpec-pointer (_ptr io _string) -> _void))

;void                gst_object_default_error            (GstObject *source, GError *error, gchar *debug);
(define-gstreamer gst_object_default_error (_fun _GstObject-pointer _GError-pointer _string -> _void))

;gboolean            gst_object_check_uniqueness         (GList *list, const gchar *name);
(define-gstreamer gst_object_check_uniqueness (_fun _GList-pointer _string -> _gboolean))

;gpointer            gst_object_ref                      (gpointer object);
(define-gstreamer gst_object_ref (_fun _gpointer -> _gpointer))

;;gpointer -> void
(define-gstreamer*
  (_fun _gpointer -> _void)
  gst_object_unref gst_object_ref_sink gst_object_sink)

;void                gst_object_replace                  (GstObject **oldobj, GstObject *newobj);
(define-gstreamer gst_object_replace (_fun (_ptr io _GstObject-pointer) _GstObject-pointer -> _void))

;guint               gst_class_signal_connect            (GstObjectClass *klass, const gchar *name, gpointer func, gpointer func_data);
(define-gstreamer gst_class_signal_connect (_fun _GstObjectClass-pointer _string _gpointer _gpointer -> _guint))
