#lang racket

(require "gst_base.rkt"
         "GstCaps-ffi.rkt"
         "GstPlugin-ffi.rkt")

(provide (all-defined-out))


#|
typedef struct {
  /* private to the caller of the typefind function */
  guint8 *  (* peek)       (gpointer data, gint64  offset, guint size);
  void      (* suggest)    (gpointer data, guint probability, const GstCaps *  caps);
  gpointer     data;
  /* optional */
  guint64   (* get_length) (gpointer data);
} GstTypeFind;
|#

(define-cstruct _GstTypeFind
  ([peek (_ptr io (_fun _gpointer _gint64 _guint -> (_ptr io _guint8)))]
   [suggest (_ptr io (_fun _gpointer _guint _GstCaps-pointer -> _void))]
   [data _gpointer]
   [get_length (_ptr io (_fun _gpointer -> _guint64))]))


;void                (*GstTypeFindFunction)              (GstTypeFind *find, gpointer data);
(define GstTypeFindFunction (_cprocedure '(_GstTypeFind-pointer _gpointer) _void))

#|
typedef enum {
  GST_TYPE_FIND_MINIMUM = 1,
  GST_TYPE_FIND_POSSIBLE = 50,
  GST_TYPE_FIND_LIKELY = 80,
  GST_TYPE_FIND_NEARLY_CERTAIN = 99,
  GST_TYPE_FIND_MAXIMUM = 100
} GstTypeFindProbability;|#

(define _GstTypeFindProbability
  (_enum '(GST_TYPE_FIND_MINIMUM = 1 GST_TYPE_FIND_POSSIBLE = 50 GST_TYPE_FIND_LIKELY = 80 GST_TYPE_FIND_NEARLY_CERTAIN = 99 GST_TYPE_FIND_MAXIMUM = 100)))


;guint8 *            gst_type_find_peek                  (GstTypeFind *find, gint64 offset, guint size);
(define-gstreamer gst_type_find_peek (_fun _GstTypeFind-pointer _gint64 _guint -> (_ptr io _guint8)))

;void                gst_type_find_suggest               (GstTypeFind *find, guint probability, const GstCaps *caps);
(define-gstreamer gst_type_find_suggest (_fun _GstTypeFind-pointer _guint _GstCaps-pointer -> _void))

;void                gst_type_find_suggest_simple        (GstTypeFind *find, guint probability, const char *media_type, const char *fieldname, ...);
(define-gstreamer gst_type_find_suggest_simple (_fun _GstTypeFind-pointer _guint _string _string (_list i _string) -> _void))

;guint64             gst_type_find_get_length            (GstTypeFind *find);
(define-gstreamer gst_type_find_get_length (_fun _GstTypeFind-pointer -> _guint64))

;gboolean            gst_type_find_register              (GstPlugin *plugin, const gchar *name, guint rank, GstTypeFindFunction func, gchar **extensions, const GstCaps *possible_caps, gpointer data, GDestroyNotify data_notify);
(define-gstreamer gst_type_find_register (_fun _GstPlugin-pointer _string _guint GstTypeFindFunction (_ptr io _string) _GstCaps-pointer _gpointer _GDestroyNotify -> _gboolean))
