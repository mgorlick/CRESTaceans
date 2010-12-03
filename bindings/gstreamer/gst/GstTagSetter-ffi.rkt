#lang racket

(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))



;void                gst_tag_setter_reset_tags           (GstTagSetter *setter);
(define-gstreamer gst_tag_setter_reset_tags (_fun _GstTagSetter-pointer -> _void))

;void                gst_tag_setter_merge_tags           (GstTagSetter *setter, const GstTagList *list, GstTagMergeMode mode);
(define-gstreamer gst_tag_setter_merge_tags (_fun _GstTagSetter-pointer _GstTagList-pointer _int -> _void))


;;GstTagSetter* GstTagMergeMode gchar* ... -> void
(define-gstreamer*
  (_fun _GstTagSetter-pointer _int _string (_list i _string) -> _void)
  gst_tag_setter_add_tags gst_tag_setter_add_tag_values)

;void                gst_tag_setter_add_tag_value        (GstTagSetter *setter, GstTagMergeMode mode, const gchar *tag, const GValue *value);
(define-gstreamer gst_tag_setter_add_tag_value (_fun _GstTagSetter-pointer _int _string _GValue-pointer -> _void))

;;GstTagSetter* GstTagMergeMode gchar* va_list -> void
(define-gstreamer*
  (_fun _GstTagSetter-pointer _int _string (_list i _string) -> _void)
  gst_tag_setter_add_tag_valist gst_tag_setter_add_tag_valist_values)

;const GstTagList *  gst_tag_setter_get_tag_list         (GstTagSetter *setter);
(define-gstreamer gst_tag_setter_get_tag_list (_fun _GstTagSetter-pointer -> _GstTagList-pointer))

;void                gst_tag_setter_set_tag_merge_mode   (GstTagSetter *setter, GstTagMergeMode mode);
(define-gstreamer gst_tag_setter_set_tag_merge_mode (_fun _GstTagSetter-pointer _int -> _void))

;GstTagMergeMode     gst_tag_setter_get_tag_merge_mode   (GstTagSetter *setter);
(define-gstreamer gst_tag_setter_get_tag_merge_mode (_fun _GstTagSetter-pointer -> _int))
