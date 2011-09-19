#lang racket

(require ffi/unsafe)
(require "gst_base.rkt"
         "gst-structs-ffi.rkt"
         "GstClock-ffi.rkt")

(provide (all-defined-out))


;gboolean            (*GstStructureForeachFunc)          (GQuark field_id, const GValue *value, gpointer user_data);
(define GstStructureForeachFunc (_cprocedure (list _GQuark _GValue-pointer _gpointer) _gboolean))

;gboolean            (*GstStructureMapFunc)              (GQuark field_id, GValue *value, gpointer user_data);
(define GstStructureMapFunc (_cprocedure (list _GQuark _GValue-pointer _gpointer) _gboolean))

;GstStructure *      gst_structure_empty_new             (const gchar *name);
(define-gstreamer gst_structure_empty_new (_fun _string -> _GstStructure-pointer))

;GstStructure *      gst_structure_id_empty_new          (GQuark quark);
(define-gstreamer gst_structure_id_empty_new (_fun _GQuark -> _GstStructure-pointer))

;GstStructure *      gst_structure_new                   (const gchar *name, const gchar *firstfield, ...); ;;OJO!!
(define-gstreamer gst_structure_new (_fun _string _string (_list i _string) -> _GstStructure-pointer))

;GstStructure *      gst_structure_new_valist            (const gchar *name, const gchar *firstfield, va_list varargs);  OJO!!!
(define-gstreamer gst_structure_new_valist (_fun _string _string (_list i _string) -> _GstStructure-pointer))

;GstStructure *      gst_structure_id_new                (GQuark name_quark, GQuark field_quark, ...); ;;OJO
(define-gstreamer gst_structure_id_new (_fun _GQuark _GQuark (_list i _GQuark) -> _GstStructure-pointer))

;GstStructure *      gst_structure_copy                  (const GstStructure *structure);
(define-gstreamer gst_structure_copy (_fun _GstStructure-pointer -> _GstStructure-pointer))

;;GstStructure* -> void
(define-gstreamer*
  (_fun _GstStructure-pointer -> _void)
  gst_structure_free gst_structure_remove_all_fields)

;const gchar *       gst_structure_get_name              (const GstStructure *structure);
(define-gstreamer gst_structure_get_name (_fun _GstStructure-pointer -> _string))

;gboolean            gst_structure_has_name              (const GstStructure *structure, const gchar *name);
(define-gstreamer gst_structure_has_name (_fun _GstStructure-pointer _string -> _gboolean))

;void                gst_structure_set_name              (GstStructure *structure, const gchar *name);
(define-gstreamer gst_structure_set_name (_fun _GstStructure-pointer _string -> _void))

;GQuark              gst_structure_get_name_id           (const GstStructure *structure);
(define-gstreamer gst_structure_get_name_id (_fun _GstStructure-pointer -> _GQuark))

;gboolean            gst_structure_id_get                (GstStructure *structure, GQuark first_field_id, ...); ;;OJO
(define-gstreamer gst_structure_id_get (_fun _GstStructure-pointer _GQuark (_list i _GQuark) -> _gboolean))

;gboolean            gst_structure_id_get_valist         (GstStructure *structure, GQuark first_field_id, va_list args); OJO!!!
(define-gstreamer gst_structure_id_get_valist (_fun _GstStructure-pointer _GQuark (_list i _GQuark) -> _gboolean))

;const GValue *      gst_structure_id_get_value          (const GstStructure *structure, GQuark field);
(define-gstreamer gst_structure_id_get_value (_fun _GstStructure-pointer _GQuark -> _GValue-pointer))

;void                gst_structure_id_set_value          (GstStructure *structure, GQuark field, const GValue *value);
(define-gstreamer gst_structure_id_set_value (_fun _GstStructure-pointer _GQuark _GValue-pointer -> _void))

;gboolean            gst_structure_get                   (GstStructure *structure, const char *first_fieldname, ...); OJO!!
(define-gstreamer gst_structure_get (_fun _GstStructure-pointer _string (_list i _string) -> _gboolean))

;gboolean            gst_structure_get_valist            (GstStructure *structure, const char *first_fieldname, va_list args); OJO!!
(define-gstreamer gst_structure_get_valist (_fun _GstStructure-pointer _string (_list i _string) -> _gboolean))

;const GValue *      gst_structure_get_value             (const GstStructure *structure, const gchar *fieldname);
(define-gstreamer gst_structure_get_value (_fun _GstStructure-pointer _string -> _GValue-pointer))

;void                gst_structure_set_value             (GstStructure *structure, const gchar *fieldname, const GValue *value);
(define-gstreamer gst_structure_set_value (_fun _GstStructure-pointer _string _GValue-pointer -> _void))

;;GstStructure* gchar* ... -> void
(define-gstreamer*
  (_fun _GstStructure-pointer _string (_list i _string) -> _void)
  gst_structure_set gst_structure_remove_fields)

;;GstStructure* gchar* va_list -> void
(define-gstreamer*
  (_fun _GstStructure-pointer _string (_list i _string) -> _void)
  gst_structure_set_valist gst_structure_remove_fields_valist)

;void                gst_structure_id_set                (GstStructure *structure, GQuark fieldname, ...);  ;;OJO!
(define-gstreamer gst_structure_id_set (_fun _GstStructure-pointer _GQuark (_list i _GQuark) -> _void))

;void                gst_structure_id_set_valist         (GstStructure *structure, GQuark fieldname, va_list varargs);  ;;OJO
(define-gstreamer gst_structure_id_set_valist (_fun _GstStructure-pointer _GQuark (_list i _GQuark) -> _void))

;void                gst_structure_remove_field          (GstStructure *structure, const gchar *fieldname);
(define-gstreamer gst_structure_remove_field (_fun _GstStructure-pointer _string -> _void))

;GType               gst_structure_get_field_type        (const GstStructure *structure, const gchar *fieldname);
(define-gstreamer gst_structure_get_field_type (_fun _GstStructure-pointer _string -> _GType))

;gboolean            gst_structure_foreach               (const GstStructure *structure, GstStructureForeachFunc func, gpointer user_data);
(define-gstreamer gst_structure_foreach (_fun _GstStructure-pointer GstStructureForeachFunc _gpointer -> _gboolean))

;gint                gst_structure_n_fields              (const GstStructure *structure);
(define-gstreamer gst_structure_n_fields (_fun _GstStructure-pointer -> _gint))

;gboolean            gst_structure_has_field             (const GstStructure *structure, const gchar *fieldname);
(define-gstreamer gst_structure_has_field (_fun _GstStructure-pointer _string -> _gboolean))

;gboolean            gst_structure_has_field_typed       (const GstStructure *structure, const gchar *fieldname, GType type);
(define-gstreamer gst_structure_has_field_typed (_fun _GstStructure-pointer _string _GType -> _gboolean))

;gboolean            gst_structure_id_has_field          (const GstStructure *structure, GQuark field);
(define-gstreamer gst_structure_id_has_field (_fun _GstStructure-pointer _GQuark -> _gboolean))

;gboolean            gst_structure_id_has_field_typed    (const GstStructure *structure, GQuark field, GType type);
(define-gstreamer gst_structure_id_has_field_typed (_fun _GstStructure-pointer _GQuark _GType -> _gboolean))

;gboolean            gst_structure_get_boolean           (const GstStructure *structure, const gchar *fieldname, gboolean *value);
(define-gstreamer gst_structure_get_boolean (_fun _GstStructure-pointer _string _gboolean -> _gboolean))

;gboolean            gst_structure_get_int               (const GstStructure *structure, const gchar *fieldname, gint *value);
(define-gstreamer gst_structure_get_int (_fun _GstStructure-pointer _string _gint -> _gboolean))

;gboolean            gst_structure_get_uint              (const GstStructure *structure, const gchar *fieldname, guint *value);
(define-gstreamer gst_structure_get_uint (_fun _GstStructure-pointer _string (_ptr io _guint) -> _gboolean))

;gboolean            gst_structure_get_fourcc            (const GstStructure *structure, const gchar *fieldname, guint32 *value);
(define-gstreamer gst_structure_get_fourcc (_fun _GstStructure-pointer _string (_ptr io _guint32) -> _gboolean))

;gboolean            gst_structure_get_double            (const GstStructure *structure, const gchar *fieldname, gdouble *value);
(define-gstreamer gst_structure_get_double (_fun _GstStructure-pointer _string (_ptr io _gdouble) -> _gboolean))

;const gchar *       gst_structure_get_string            (const GstStructure *structure, const gchar *fieldname);
(define-gstreamer gst_structure_get_string (_fun _GstStructure-pointer _string -> _string))

;gboolean            gst_structure_get_date              (const GstStructure *structure, const gchar *fieldname, GDate **value);
(define-gstreamer gst_structure_get_date (_fun _GstStructure-pointer _string (_ptr io _GDate-pointer) -> _gboolean))

;gboolean            gst_structure_get_clock_time        (const GstStructure *structure, const gchar *fieldname, GstClockTime *value);
(define-gstreamer gst_structure_get_clock_time (_fun _GstStructure-pointer _string (_ptr io _GstClockTime) -> _gboolean))

;gboolean            gst_structure_get_enum              (const GstStructure *structure, const gchar *fieldname, GType enumtype, gint *value);
(define-gstreamer gst_structure_get_enum (_fun _GstStructure-pointer _string _GType (_ptr io _gint) -> _gboolean))

;gboolean            gst_structure_get_fraction          (const GstStructure *structure, const gchar *fieldname, gint *value_numerator, gint *value_denominator);
(define-gstreamer gst_structure_get_fraction (_fun _GstStructure-pointer _string (_ptr io _gint) (_ptr io _gint) -> _gboolean))

;gboolean            gst_structure_map_in_place          (GstStructure *structure, GstStructureMapFunc func, gpointer user_data);
(define-gstreamer gst_structure_map_in_place (_fun _GstStructure-pointer GstStructureMapFunc _gpointer -> _gboolean))

;const gchar *       gst_structure_nth_field_name        (const GstStructure *structure, guint index);
(define-gstreamer gst_structure_nth_field_name (_fun _GstStructure-pointer _guint -> _string))

;void                gst_structure_set_parent_refcount   (GstStructure *structure, gint *refcount);
(define-gstreamer gst_structure_set_parent_refcount (_fun _GstStructure-pointer (_or-null (_ptr io _gint)) -> _void))

;gchar *             gst_structure_to_string             (const GstStructure *structure);
(define-gstreamer gst_structure_to_string (_fun _GstStructure-pointer -> _string))

;GstStructure *      gst_structure_from_string           (const gchar *string, gchar **end);
(define-gstreamer gst_structure_from_string (_fun _string (_ptr io _string) -> _GstStructure-pointer))

;gboolean            gst_structure_fixate_field_nearest_int (GstStructure *structure, const char *field_name, int target);
(define-gstreamer gst_structure_fixate_field_nearest_int (_fun _GstStructure-pointer _string _int -> _gboolean))

;gboolean            gst_structure_fixate_field_nearest_double (GstStructure *structure, const char *field_name, double target);
(define-gstreamer gst_structure_fixate_field_nearest_double (_fun _GstStructure-pointer _string _double -> _gboolean))

;gboolean            gst_structure_fixate_field_nearest_fraction (GstStructure *structure, const char *field_name, const gint target_numerator, const gint target_denominator);
(define-gstreamer gst_structure_fixate_field_nearest_fraction (_fun _GstStructure-pointer _string _gint _gint -> _gboolean))

;gboolean            gst_structure_fixate_field_boolean  (GstStructure *structure, const char *field_name, gboolean target);
(define-gstreamer gst_structure_fixate_field_boolean (_fun _GstStructure-pointer _string _gboolean -> _gboolean))

;;NOT IN LIB
;gboolean            gst_structure_fixate_field_string   (GstStructure *structure, const char *field_name, const gchar *target);
;(define-gstreamer gst_structure_fixate_field_string (_fun _GstStructure-pointer _string _string -> _gboolean))
