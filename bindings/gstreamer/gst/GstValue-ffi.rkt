#lang racket

(require ffi/unsafe)
(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))


;gint                (*GstValueCompareFunc)              (const GValue *value1, const GValue *value2); OJO
(define GstValueCompareFunc (_cprocedure (list _GValue-pointer _GValue-pointer) _gint))

;gchar *             (*GstValueSerializeFunc)            (const GValue *value1);
(define GstValueSerializeFunc (_cprocedure (list _GValue-pointer) _string))

;gboolean            (*GstValueDeserializeFunc)          (GValue *dest, const gchar *s);
(define GstValueDeserializeFunc (_cprocedure (list _GValue-pointer _string) _gboolean))

;gboolean            (*GstValueUnionFunc)                (GValue *dest, const GValue *value1, const GValue *value2);
(define GstValueUnionFunc (_cprocedure (list _GValue-pointer _GValue-pointer _GValue-pointer) _gboolean))

;gboolean            (*GstValueIntersectFunc)            (GValue *dest, const GValue *value1, const GValue *value2);
(define GstValueIntersectFunc (_cprocedure (list _GValue-pointer _GValue-pointer _GValue-pointer) _gboolean))

;gboolean            (*GstValueSubtractFunc)             (GValue *dest, const GValue *minuend, const GValue *subtrahend);
(define GstValueSubtractFunc (_cprocedure (list _GValue-pointer _GValue-pointer _GValue-pointer) _gboolean))


;#define             GST_VALUE_HOLDS_FOURCC              (x)

;#define GST_MAKE_FOURCC(a,b,c,d)        ((guint32)((a)|(b)<<8|(c)<<16|(d)<<24))
;(define-syntax-rule (GST_MAKE_FOURCC a b c d)
;  cast here (bitwise-ior a (arithmetic-shift b 8) (arithmetic-shift c 16) (arithmetic-shift d 24)))

;#define             GST_STR_FOURCC                      (f)
;(define GST_STR_FOURCC '(f))

;#define             GST_FOURCC_FORMAT
;(define GST_FOURCC_FORMAT "c%c%c%c")

;#define             GST_FOURCC_ARGS                     (fourcc)
;(define GST_FOURCC_ARGS '(fourcc))

;#define             GST_TYPE_FOURCC
;(define GST_TYPE_FOURCC gst_fourcc_get_type())

;#define             GST_VALUE_HOLDS_INT_RANGE           (x)
;#define             GST_TYPE_INT_RANGE
;#define             GST_VALUE_HOLDS_DOUBLE_RANGE        (x)
;#define             GST_TYPE_DOUBLE_RANGE
;#define             GST_VALUE_HOLDS_LIST                (x)
;#define             GST_TYPE_LIST
;#define             GST_VALUE_HOLDS_ARRAY               (x)
;#define             GST_TYPE_ARRAY
;#define             GST_VALUE_HOLDS_FRACTION            (x)
;#define             GST_TYPE_FRACTION
;#define             GST_VALUE_HOLDS_FRACTION_RANGE      (x)
;#define             GST_TYPE_FRACTION_RANGE
;#define             GST_VALUE_HOLDS_DATE                (x)
;#define             GST_TYPE_DATE
;#define             GST_VALUE_HOLDS_CAPS                (x)
;#define             GST_VALUE_HOLDS_STRUCTURE           (x)
;#define             GST_VALUE_HOLDS_MINI_OBJECT         (value)
;#define             GST_VALUE_HOLDS_BUFFER              (x)
;#define             gst_value_get_buffer                (v)
;#define             gst_value_set_buffer                (v, b)
;#define             gst_value_take_buffer               (v, b)
;#define             GST_VALUE_LESS_THAN
;#define             GST_VALUE_EQUAL
;#define             GST_VALUE_GREATER_THAN
;#define             GST_VALUE_UNORDERED


;void                gst_value_set_fourcc                (GValue *value, guint32 fourcc);
(define-gstreamer gst_value_set_fourcc (_fun _GValue-pointer _guint32 -> _void))

;guint32             gst_value_get_fourcc                (const GValue *value);
(define-gstreamer gst_value_get_fourcc (_fun _GValue-pointer -> _guint32))

;void                gst_value_set_int_range             (GValue *value, gint start, gint end);
(define-gstreamer gst_value_set_int_range (_fun _GValue-pointer _gint _gint -> _void))

;;GValue* -> gint
(define-gstreamer*
  (_fun _GValue-pointer -> _gint)
  gst_value_get_int_range_min gst_value_get_int_range_max)

;void                gst_value_set_double_range          (GValue *value, gdouble start, gdouble end);
(define-gstreamer gst_value_set_double_range (_fun _GValue-pointer _gdouble _gdouble -> _void))

;;GValue* -> gdouble
(define-gstreamer*
  (_fun _GValue-pointer -> _gdouble)
  gst_value_get_double_range_min gst_value_get_double_range_max)

;void                gst_value_list_append_value         (GValue *value, const GValue *append_value);
(define-gstreamer gst_value_list_append_value (_fun _GValue-pointer _GValue-pointer -> _void))

;void                gst_value_list_prepend_value        (GValue *value, const GValue *prepend_value);
(define-gstreamer gst_value_list_prepend_value (_fun _GValue-pointer _GValue-pointer -> _void))

;;GValue* GValue* GValue* -> void
(define-gstreamer*
  (_fun _GValue-pointer _GValue-pointer _GValue-pointer -> _void)
  gst_value_list_concat gst_value_set_fraction_range)

;;GValue* -> guint
(define-gstreamer*
  (_fun _GValue-pointer -> _guint)
  gst_value_list_get_size gst_value_array_get_size)

;const GValue *      gst_value_list_get_value            (const GValue *value, guint index);
(define-gstreamer gst_value_list_get_value (_fun _GValue-pointer _guint -> _GValue-pointer))

;void                gst_value_set_fraction              (GValue *value, gint numerator, gint denominator);
(define-gstreamer gst_value_set_fraction (_fun _GValue-pointer _gint _gint -> _void))

;;GValue* -> gint
(define-gstreamer*
  (_fun _GValue-pointer -> _gint)
  gst_value_get_fraction_numerator gst_value_get_fraction_denominator)

;;GValue* GValue* GValue* -> gboolean
(define-gstreamer*
  (_fun _GValue-pointer _GValue-pointer _GValue-pointer -> _gboolean)
  gst_value_fraction_multiply gst_value_fraction_subtract gst_value_union gst_value_intersect gst_value_subtract)

;;GValue* -> GValue*
(define-gstreamer*
  (_fun _GValue-pointer -> _GValue-pointer)
  gst_value_get_fraction_range_min gst_value_get_fraction_range_max)

;void                gst_value_set_fraction_range_full   (GValue *value, gint numerator_start, gint denominator_start, gint numerator_end, gint denominator_end);
(define-gstreamer gst_value_set_fraction_range_full (_fun _GValue-pointer _gint _gint _gint _gint -> _void))

;void                gst_value_set_date                  (GValue *value, const GDate *date);
(define-gstreamer gst_value_set_date (_fun _GValue-pointer _GDate-pointer -> _void))

;const GDate *       gst_value_get_date                  (const GValue *value);
(define-gstreamer gst_value_get_date (_fun _GValue-pointer -> _GDate-pointer))

;void                gst_value_set_caps                  (GValue *value, const GstCaps *caps);
(define-gstreamer gst_value_set_caps (_fun _GValue-pointer _GstCaps-pointer -> _void))

;const GstCaps *     gst_value_get_caps                  (const GValue *value);
(define-gstreamer gst_value_get_caps (_fun _GValue-pointer -> _GstCaps-pointer))

;void                gst_value_set_structure             (GValue *value, const GstStructure *structure);
(define-gstreamer gst_value_set_structure (_fun _GValue-pointer _GstStructure-pointer -> _void))

;const GstStructure * gst_value_get_structure            (const GValue *value);
(define-gstreamer gst_value_get_structure (_fun _GValue-pointer -> _GstStructure-pointer))



;gint                gst_value_compare                   (const GValue *value1, const GValue *value2);
(define-gstreamer gst_value_compare (_fun _GValue-pointer _GValue-pointer -> _gint))



;gchar *             gst_value_serialize                 (const GValue *value);
(define-gstreamer gst_value_serialize (_fun _GValue-pointer -> _string))


;gboolean            gst_value_is_fixed                  (const GValue *value);
(define-gstreamer gst_value_is_fixed (_fun _GValue-pointer -> _gboolean))

;void                gst_value_register                  (const GstValueTable *table);
(define-gstreamer gst_value_register (_fun _GstValueTable-pointer -> _void))

;void                gst_value_init_and_copy             (GValue *dest, const GValue *src);
(define-gstreamer gst_value_init_and_copy (_fun _GValue-pointer _GValue-pointer -> _void))

;gboolean            gst_value_deserialize               (GValue *dest, const gchar *src);
(define-gstreamer gst_value_deserialize (_fun _GValue-pointer _string -> _gboolean))

;gboolean            gst_value_can_compare               (const GValue *value1, const GValue *value2);
(define-gstreamer gst_value_can_compare (_fun _GValue-pointer _GValue-pointer -> _gboolean))

;;GValue* GValue* -> gboolean
(define-gstreamer*
  (_fun _GValue-pointer _GValue-pointer -> _gboolean)
  gst_value_can_union gst_value_can_subtract gst_value_can_intersect)

;void                gst_value_register_union_func       (GType type1, GType type2, GstValueUnionFunc func);
(define-gstreamer  gst_value_register_union_func (_fun _GType _GType GstValueUnionFunc -> _void))

;void                gst_value_register_subtract_func    (GType minuend_type, GType subtrahend_type, GstValueSubtractFunc func);
(define-gstreamer  gst_value_register_subtract_func (_fun _GType _GType GstValueSubtractFunc -> _void))

;void                gst_value_register_intersect_func   (GType type1, GType type2, GstValueIntersectFunc func);
(define-gstreamer  gst_value_register_intersect_func (_fun _GType _GType GstValueIntersectFunc -> _void))

;;GValue* GValue* -> void
(define-gstreamer*
  (_fun _GValue-pointer _GValue-pointer -> _void)
  gst_value_array_append_value gst_value_array_prepend_value)

;const GValue *      gst_value_array_get_value           (const GValue *value, guint index);
(define-gstreamer  gst_value_array_get_value (_fun _GValue-pointer _guint -> _GValue-pointer))


