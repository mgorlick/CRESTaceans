#lang racket

(require ffi/unsafe)
(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))


#|typedef enum
{
  GST_MINI_OBJECT_FLAG_READONLY = (1<<0),
  /* padding */
  GST_MINI_OBJECT_FLAG_LAST = (1<<4)
} GstMiniObjectFlags;|#

(define GST_MINI_OBJECT_FLAG_READONLY (arithmetic-shift 1 0))
(define GST_MINI_OBJECT_FLAG_LAST (arithmetic-shift 1 4))


#|#define             GST_MINI_OBJECT_FLAGS               (obj)
#define             GST_MINI_OBJECT_FLAG_IS_SET         (obj, flag)
#define             GST_MINI_OBJECT_FLAG_SET            (obj, flag)
#define             GST_MINI_OBJECT_FLAG_UNSET          (obj, flag)
#define             GST_MINI_OBJECT_REFCOUNT            (obj)
#define             GST_MINI_OBJECT_REFCOUNT_VALUE      (obj)|#


;GstMiniObject *     (*GstMiniObjectCopyFunction)        (const GstMiniObject *obj);
(define GstMiniObjectCopyFunction (_cprocedure (list _GstMiniObject-pointer) _GstMiniObject-pointer))

;void                (*GstMiniObjectFinalizeFunction)    (GstMiniObject *obj);
(define GstMiniObjectFinalizeFunction (_cprocedure (list _GstMiniObject-pointer) _void))

;GstMiniObject*      gst_mini_object_new                 (GType type);
(define-gstreamer gst_mini_object_new (_fun _GType -> _GstMiniObject-pointer))

;GstMiniObject*      gst_mini_object_copy                (const GstMiniObject *mini_object);
(define-gstreamer gst_mini_object_copy (_fun _GstMiniObject-pointer -> _GstMiniObject-pointer))

;gboolean            gst_mini_object_is_writable         (const GstMiniObject *mini_object);
(define-gstreamer gst_mini_object_is_writable (_fun _GstMiniObject-pointer -> _gboolean))


;;GstMiniObject* -> GstMiniObject*
(define-gstreamer*
  (_fun _GstMiniObject-pointer -> _GstMiniObject-pointer)
  gst_mini_object_make_writable gst_mini_object_ref)

;void                gst_mini_object_unref               (GstMiniObject *mini_object);
(define-gstreamer gst_mini_object_unref (_fun _GstMiniObject-pointer -> _void))

;void                gst_mini_object_replace             (GstMiniObject **olddata, GstMiniObject *newdata);
(define-gstreamer gst_mini_object_replace (_fun (_ptr io _GstMiniObject-pointer) _GstMiniObject-pointer -> _void))

#|typedef struct {
  GParamSpec parent_instance;
} GstParamSpecMiniObject;|#

(define-cpointer-type _GstParamSpecMiniObject-pointer)

  
;GParamSpec*         gst_param_spec_mini_object          (const char *name, const char *nick, const char *blurb, GType object_type, GParamFlags flags);
(define-gstreamer gst_param_spec_mini_object (_fun _string _string _string _GType _int -> _GParamSpec-pointer))

;;GValue* GstMiniObject* -> void
(define-gstreamer*
  (_fun _GValue-pointer _GstMiniObject-pointer -> _void)
  gst_value_set_mini_object gst_value_take_mini_object)

;;GValue* -> GstMiniObject*
(define-gstreamer*
  (_fun _GValue-pointer -> _GstMiniObject-pointer)
  gst_value_get_mini_object gst_value_dup_mini_object)
