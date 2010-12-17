#lang racket

(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))

;;GstPreset* -> gchar**
(define-gstreamer*
  (_fun _GstPreset-pointer -> (_ptr io _string)
  gst_preset_get_preset_names gst_preset_get_property_names))
  
  ;;GstPreset* gchar* -> gboolean
(define-gstreamer*
  (_fun _GstPreset-pointer _string -> gboolean
  gst_preset_load_preset gst_preset_save_preset gst_preset_delete_preset))

;gboolean            gst_preset_rename_preset            (GstPreset *preset, const gchar *old_name, const gchar *new_name);
(define-gstreamer gst_preset_rename_preset (_fun _GstPreset-pointer _string _string -> _gboolean))

;gboolean            gst_preset_set_meta                 (GstPreset *preset, const gchar *name, const gchar *tag, const gchar *value);
(define-gstreamer gst_preset_set_meta (_fun _GstPreset-pointer _string _string _string -> _gboolean))

;gboolean            gst_preset_get_meta                 (GstPreset *preset, const gchar *name, const gchar *tag, gchar **value);
(define-gstreamer gst_preset_get_meta (_fun _GstPreset-pointer _string _string (_ptr io _string) -> _gboolean))
