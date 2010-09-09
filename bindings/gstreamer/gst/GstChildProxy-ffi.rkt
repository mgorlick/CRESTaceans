#lang racket

(require "gst_base.rkt"
         "GstObject-ffi.rkt")

(provide (all-defined-out))

;;typedef struct _GstChildProxy GstChildProxy;      ;;Opaque
(define-cpointer-type _GstChildProxy-pointer)

#|
typedef struct {
  GTypeInterface parent;
  /* methods */
  GstObject *(*get_child_by_index) (GstChildProxy * parent, guint index);
  guint (*get_children_count) (GstChildProxy * parent);
} GstChildProxyInterface;
|#

(define-cstruct _GstChildProxyInterface
  ([parent _GTypeInterface-pointer]
   [get_child_by_index (_ptr io (_fun _GstChildProxy-pointer _guint -> _GstObject-pointer))]
   [get_children_count (_ptr io (_fun _GstChildProxy-pointer -> _guint))]))


;guint               gst_child_proxy_get_children_count  (GstChildProxy *parent);
(define-gstreamer gst_child_proxy_get_children_count (_fun _GstChildProxy-pointer -> _guint))

;GstObject *         gst_child_proxy_get_child_by_name   (GstChildProxy *parent,const gchar *name);
(define-gstreamer gst_child_proxy_get_child_by_name (_fun _GstChildProxy-pointer _string -> _GstObject-pointer))

;GstObject *         gst_child_proxy_get_child_by_index  (GstChildProxy *parent, guint index);
(define-gstreamer gst_child_proxy_get_child_by_index (_fun _GstChildProxy-pointer _guint -> _GstObject-pointer))

;gboolean            gst_child_proxy_lookup              (GstObject *object, const gchar *name, GstObject **target, GParamSpec **pspec);
(define-gstreamer gst_child_proxy_lookup (_fun _GstObject-pointer _string (_ptr io _GstObject-pointer) (_ptr io _GParamSpec-pointer) -> _gboolean))

;;GstObject* gchar* GValue* -> void
(define-gstreamer*
  (_fun _GstObject-pointer _string _GValue-pointer -> _void)
  gst_child_proxy_get_property gst_child_proxy_set_property)

;;GstObject* gchar* va_list -> void
(define-gstreamer*
  (_fun _GstObject-pointer _string (_list i _string) -> _void)
  gst_child_proxy_get_valist gst_child_proxy_set_valist)
  
;;GstObject* gchar* ... -> void
(define-gstreamer*
  (_fun _GstObject-pointer _string (_list i _string) -> _void)
  gst_child_proxy_get gst_child_proxy_set)

;;GstObject* GstObject* -> void
(define-gstreamer*
  (_fun _GstObject-pointer _GstObject-pointer -> _void)
  gst_child_proxy_child_added gst_child_proxy_child_removed)
