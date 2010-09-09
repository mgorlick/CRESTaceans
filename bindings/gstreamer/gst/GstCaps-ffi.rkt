#lang racket

(require "gst_base.rkt"
         "GstStructure-ffi.rkt")

(provide (all-defined-out))


#|
typedef enum {
  GST_CAPS_FLAGS_ANY = (1 << 0)
} GstCapsFlags;|#

(define GST_CAPS_FLAGS_ANY (arithmetic-shift 1 0))


#|
typedef struct {
  GType type;
  /* refcounting */
  gint           refcount;
  GstCapsFlags flags;
} GstCaps;
|#

(define-cstruct _GstCaps
  ([type _GType]
   [refcount _gint]
   [flags _int]))


#|
typedef struct {
  GstCaps caps;
  const char *string;
} GstStaticCaps;
|#

(define-cstruct _GstStaticCaps
  ([caps _GstCaps]
   [string _string]))


#|#define             GST_CAPS_ANY
#define             GST_CAPS_NONE
#define             GST_CAPS_REFCOUNT                   (caps)
#define             GST_CAPS_REFCOUNT_VALUE             (caps)
#define             GST_STATIC_CAPS_ANY
#define             GST_STATIC_CAPS_NONE
#define             GST_CAPS_IS_SIMPLE                  (caps)
#define             GST_STATIC_CAPS                     (string)|#


;;void -> GstCaps*
(define-gstreamer*
  (_fun -> _GstCaps-pointer)
  gst_caps_new_empty gst_caps_new_any)

;GstCaps *           gst_caps_new_simple                 (const char *media_type, const char *fieldname,...);
(define-gstreamer gst_caps_new_simple (_fun _string _string (_list i _string) -> _GstCaps-pointer))

;GstCaps *           gst_caps_new_full                   (GstStructure *struct1, ...);
(define-gstreamer gst_caps_new_full (_fun _GstStructure-pointer (_list i _GstStructure-pointer) -> _GstCaps-pointer))

;GstCaps *           gst_caps_new_full_valist            (GstStructure *structure, va_list var_args);   OJO!!!
(define-gstreamer gst_caps_new_full_valist (_fun _GstStructure-pointer (_list i _GstStructure-pointer) -> _GstCaps-pointer))

;;GstCaps* -> GstCaps*
(define-gstreamer*
  (_fun _GstCaps-pointer -> _GstCaps-pointer)
  gst_caps_copy gst_caps_make_writable gst_caps_ref)

;GstCaps *           gst_caps_copy_nth                   (const GstCaps *caps, guint nth);
(define-gstreamer gst_caps_copy_nth (_fun _GstCaps-pointer _guint -> _GstCaps-pointer))

;GstCaps *           gst_static_caps_get                 (GstStaticCaps *static_caps);
(define-gstreamer gst_static_caps_get (_fun _GstStaticCaps-pointer -> _GstCaps-pointer))


;;GstCaps* GstCaps* -> void
(define-gstreamer*
  (_fun _GstCaps-pointer _GstCaps-pointer -> _void)
  gst_caps_append gst_caps_merge)


;;GstCaps* GstStructure* -> void
(define-gstreamer*
  (_fun _GstCaps-pointer _GstStructure-pointer -> _void)
  gst_caps_append_structure gst_caps_merge_structure)


;void                gst_caps_remove_structure           (GstCaps *caps, guint idx);
(define-gstreamer gst_caps_remove_structure (_fun _GstCaps-pointer _guint -> _void))

;;GstCaps* guint -> GstStructure*
(define-gstreamer*
  (_fun _GstCaps-pointer _guint -> _GstStructure-pointer)
  gst_caps_get_structure)  ;;gst_caps_steal_structure NOT IN LIB

;guint               gst_caps_get_size                   (const GstCaps *caps);
(define-gstreamer gst_caps_get_size (_fun _GstCaps-pointer -> _guint))

;void                gst_caps_set_value                  (GstCaps *caps, const char *field, const GValue *value);
(define-gstreamer gst_caps_set_value (_fun _GstCaps-pointer _string _GValue-pointer -> _void))

;void                gst_caps_set_simple                 (GstCaps *caps, const char *field, ...);
(define-gstreamer gst_caps_set_simple (_fun _GstCaps-pointer _string (_list i _string) -> _void))

;void                gst_caps_set_simple_valist          (GstCaps *caps, const char *field, va_list varargs); OJO!!!
(define-gstreamer gst_caps_set_simple_valist (_fun _GstCaps-pointer _string (_list i _string) -> _void))


;;GstCaps* -> gboolean
(define-gstreamer*
  (_fun _GstCaps-pointer -> _gboolean)
  gst_caps_is_any gst_caps_is_empty gst_caps_is_fixed gst_caps_do_simplify)


;;GstCaps* GstCaps* -> gboolean
(define-gstreamer*
  (_fun _GstCaps-pointer _GstCaps-pointer -> _gboolean)
  gst_caps_is_equal gst_caps_is_equal_fixed gst_caps_is_always_compatible gst_caps_can_intersect gst_caps_is_subset)


;;GstCaps* GstCaps* -> GstCaps*
(define-gstreamer*
  (_fun _GstCaps-pointer _GstCaps-pointer -> _GstCaps-pointer)
  gst_caps_intersect gst_caps_union)


;xmlNodePtr          gst_caps_save_thyself               (const GstCaps *caps, xmlNodePtr parent);
;(define-gstreamer gst_caps_save_thyself (_fun _GstCaps-pointer _xmlNodePtr -> _xmlNodePtr))

;GstCaps *           gst_caps_load_thyself               (xmlNodePtr parent);
;(define-gstreamer gst_caps_load_thyself (_fun _xmlNodePtr -> _GstCaps-pointer))

;void                gst_caps_replace                    (GstCaps **caps, GstCaps *newcaps);
(define-gstreamer gst_caps_replace (_fun (_ptr io _GstCaps-pointer) _GstCaps-pointer -> _void))

;gchar *             gst_caps_to_string                  (const GstCaps *caps);
(define-gstreamer gst_caps_to_string (_fun _GstCaps-pointer -> _string))

;GstCaps *           gst_caps_from_string                (const gchar *string);
(define-gstreamer gst_caps_from_string (_fun _string -> _GstCaps-pointer))

;GstCaps *           gst_caps_subtract                   (const GstCaps *minuend, const GstCaps *subtrahend);
(define-gstreamer gst_caps_subtract (_fun _GstCaps-pointer _GstCaps-pointer -> _GstCaps-pointer))

;;GstCaps* -> void
(define-gstreamer*
  (_fun _GstCaps-pointer -> _void)
  gst_caps_truncate gst_caps_unref)