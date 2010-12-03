#lang racket

(require "gst_base.rkt"
         "gst-structs-ffi.rkt"
         "GstClock-ffi.rkt")

(provide (all-defined-out))


#|#define             GST_BOILERPLATE_WITH_INTERFACE      (type, type_as_function, parent_type, parent_type_as_macro, interface_type, interface_type_as_macro, interface_as_function)
#define             GST_BOILERPLATE_FULL                (type, type_as_function, parent_type, parent_type_macro, additional_initializations)
#define             GST_BOILERPLATE                     (type,type_as_function, parent_type, parent_type_macro)
#define             GST_CALL_PARENT                     (parent_class_cast, name, args)
#define             GST_CALL_PARENT_WITH_DEFAULT        (parent_class_cast, name, args, def_return)
#define             GDOUBLE_FROM_BE                     (val)
#define             GDOUBLE_FROM_LE                     (val)
#define             GDOUBLE_TO_BE                       (val)
#define             GDOUBLE_TO_LE                       (val)
gdouble             GDOUBLE_SWAP_LE_BE                  (gdouble in);
#define             GFLOAT_FROM_BE                      (val)
#define             GFLOAT_FROM_LE                      (val)
#define             GFLOAT_TO_BE                        (val)
#define             GFLOAT_TO_LE                        (val)
gfloat              GFLOAT_SWAP_LE_BE                   (gfloat in);
#define             GST_READ_UINT8                      (data)
#define             GST_READ_UINT16_LE                  (data)
#define             GST_READ_UINT16_BE                  (data)
#define             GST_READ_UINT24_LE                  (data)
#define             GST_READ_UINT24_BE                  (data)
#define             GST_READ_UINT32_LE                  (data)
#define             GST_READ_UINT32_BE                  (data)
#define             GST_READ_UINT64_LE                  (data)
#define             GST_READ_UINT64_BE                  (data)
gfloat              GST_READ_FLOAT_LE                   (const guint8 *data);
gfloat              GST_READ_FLOAT_BE                   (const guint8 *data);
gdouble             GST_READ_DOUBLE_LE                  (const guint8 *data);
gdouble             GST_READ_DOUBLE_BE                  (const guint8 *data);
#define             GST_WRITE_UINT8                     (data, num)
#define             GST_WRITE_UINT16_LE                 (data, num)
#define             GST_WRITE_UINT16_BE                 (data, num)
#define             GST_WRITE_UINT24_LE                 (data, num)
#define             GST_WRITE_UINT24_BE                 (data, num)
#define             GST_WRITE_UINT32_LE                 (data, num)
#define             GST_WRITE_UINT32_BE                 (data, num)
#define             GST_WRITE_UINT64_LE                 (data, num)
#define             GST_WRITE_UINT64_BE                 (data, num)
void                GST_WRITE_FLOAT_LE                  (guint8 *data, gfloat num);
void                GST_WRITE_FLOAT_BE                  (guint8 *data, gfloat num);
void                GST_WRITE_DOUBLE_LE                 (guint8 *data, gdouble num);
void                GST_WRITE_DOUBLE_BE                 (guint8 *data, gdouble num);
#define             GST_ROUND_UP_2                      (num)
#define             GST_ROUND_UP_4                      (num)
#define             GST_ROUND_UP_8                      (num)
#define             GST_ROUND_UP_16                     (num)
#define             GST_ROUND_UP_32                     (num)
#define             GST_ROUND_UP_64                     (num)
#define             GST_ROUND_DOWN_2                    (num)
#define             GST_ROUND_DOWN_4                    (num)
#define             GST_ROUND_DOWN_8                    (num)
#define             GST_ROUND_DOWN_16                   (num)
#define             GST_ROUND_DOWN_32                   (num)
#define             GST_ROUND_DOWN_64                   (num)
#define             gst_guint64_to_gdouble              (value)
#define             gst_gdouble_to_guint64              (value)|#


;const gchar*        gst_flow_get_name                   (GstFlowReturn ret);
(define-gstreamer gst_flow_get_name (_fun _int -> _string))

;GQuark              gst_flow_to_quark                   (GstFlowReturn ret);
(define-gstreamer gst_flow_to_quark (_fun _int -> _GQuark))

;void                gst_print_element_args              (GString *buf, gint indent, GstElement *element);
(define-gstreamer gst_print_element_args (_fun _GString-pointer _gint _GstElement-pointer -> _void))

;void                gst_print_pad_caps                  (GString *buf, gint indent, GstPad *pad);
(define-gstreamer gst_print_pad_caps (_fun _GString-pointer _gint _GstPad-pointer -> _void))

;GType gst_type_register_static_full       (GType parent_type, const gchar *type_name, guint class_size, GBaseInitFunc base_init, GBaseFinalizeFunc base_finalize, GClassInitFunc class_init, GClassFinalizeFunc class_finalize, gconstpointer class_data, guint instance_size, guint16 n_preallocs, GInstanceInitFunc instance_init, const GTypeValueTable *value_table, GTypeFlags flags);
(define-gstreamer gst_type_register_static_full (_fun _GType _string _guint GBaseInitFunc GBaseFinalizeFunc GClassInitFunc GClassFinalizeFunc _gconstpointer _guint _guint16 GInstanceInitFunc _GTypeValueTable-pointer _int -> _GType))

;void                gst_util_dump_mem                   (const guchar *mem, guint size);
(define-gstreamer gst_util_dump_mem (_fun (_ptr io _guchar) _guint -> _void))

;;guint64 guint64 guint64 -> guint64
(define-gstreamer*
  (_fun _guint64 _guint64 _guint64 -> _guint64)
  gst_util_uint64_scale gst_util_uint64_scale_round gst_util_uint64_scale_ceil)


;;guint64 gint gint -> guint64
(define-gstreamer*
  (_fun _guint64 _gint _gint -> _guint64)
  gst_util_uint64_scale_int gst_util_uint64_scale_int_round gst_util_uint64_scale_int_ceil)

;gint                gst_util_greatest_common_divisor    (gint a, gint b);
(define-gstreamer gst_util_greatest_common_divisor (_fun _gint _gint -> _gint))

;void                gst_util_fraction_to_double         (gint src_n, gint src_d, gdouble *dest);
(define-gstreamer gst_util_fraction_to_double (_fun _gint _gint (_ptr io _gdouble) -> _void))

;void                gst_util_double_to_fraction         (gdouble src, gint *dest_n, gint *dest_d);
(define-gstreamer gst_util_double_to_fraction (_fun _gdouble (_ptr io _gint) (_ptr io _gint) -> _void))

;;gint gint gint gint gint* gint* -> gboolean
(define-gstreamer*
  (_fun _gint _gint _gint _gint (_ptr io _gint) (_ptr io _gint) -> _gboolean)
  gst_util_fraction_multiply gst_util_fraction_add)

;guint32             gst_util_seqnum_next                (void);
(define-gstreamer gst_util_seqnum_next (_fun -> _guint32))

;gint32              gst_util_seqnum_compare             (guint32 s1, guint32 s2);
(define-gstreamer gst_util_seqnum_compare (_fun _guint32 _guint32 -> _gint32))

;void                gst_util_set_object_arg             (GObject *object, const gchar *name, const gchar *value);
(define-gstreamer gst_util_set_object_arg (_fun _GObject-pointer _string _string -> _void))

;void                gst_util_set_value_from_string      (GValue *value, const gchar *value_str);
(define-gstreamer gst_util_set_value_from_string (_fun _GValue-pointer _string -> _void))

;GstClockTime        gst_util_get_timestamp              (void);
(define-gstreamer gst_util_get_timestamp (_fun -> _GstClockTime))

#|typedef enum {
  GST_SEARCH_MODE_EXACT = 0,
  GST_SEARCH_MODE_BEFORE,
  GST_SEARCH_MODE_AFTER
} GstSearchMode;|#

(define GST_SEARCH_MODE_EXACT 0)
(define GST_SEARCH_MODE_BEFORE 1)
(define GST_SEARCH_MODE_AFTER 2)

;gpointer gst_util_array_binary_search (gpointer array, guint num_elements, gsize element_size, GCompareDataFunc search_func, GstSearchMode mode, gconstpointer search_data, gpointer user_data);
(define-gstreamer gst_util_array_binary_search (_fun _gpointer _guint _gsize GCompareDataFunc _int _gconstpointer _gpointer -> _gpointer))
