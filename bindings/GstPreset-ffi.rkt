#lang racket

(require "gst_base.rkt")

(provide (all-defined-out))

;;typedef struct _GstPreset GstPreset;
(define-cpointer-type _GstPreset-pointer)

#|
typedef struct {
  GTypeInterface parent;
  /* methods */
  gchar**      (*get_preset_names)    (GstPreset *preset);
  gchar**      (*get_property_names)  (GstPreset *preset);
  gboolean     (*load_preset)         (GstPreset *preset, const gchar *name);
  gboolean     (*save_preset)         (GstPreset *preset, const gchar *name);
  gboolean     (*rename_preset)       (GstPreset *preset, const gchar *old_name, const gchar *new_name);
  gboolean     (*delete_preset)       (GstPreset *preset, const gchar *name); 
  gboolean     (*set_meta)            (GstPreset *preset, const gchar *name, const gchar *tag, const gchar *value);
  gboolean     (*get_meta)            (GstPreset *preset, const gchar *name, const gchar *tag, gchar **value);
} GstPresetInterface;
|#

(define-cstruct _GstPresetInterface
  ([parent _GTypeInterface-pointer]
   [get_preset_names (_ptr io (_fun _GstPreset-pointer -> (_ptr io _string)))]
   [get_property_names (_ptr io (_fun _GstPreset-pointer -> (_ptr io _string)))]
   [load_preset (_ptr io (_fun _GstPreset-pointer _string -> _gboolean))]
   [save_preset (_ptr io (_fun _GstPreset-pointer _string -> _gboolean))]
   [rename_preset (_ptr io (_fun _GstPreset-pointer _string _string -> _gboolean))]
   [delete_preset (_ptr io (_fun _GstPreset-pointer _string -> _gboolean))]
   [set_meta (_ptr io (_fun _GstPreset-pointer _string _string _string -> _gboolean))]
   [get_meta (_ptr io (_fun _GstPreset-pointer _string _string (_ptr io _string) -> _gboolean))]))

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
