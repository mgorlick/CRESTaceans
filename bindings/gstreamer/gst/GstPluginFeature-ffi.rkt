#lang racket

(require ffi/unsafe)
(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))


#|#define             GST_PLUGIN_FEATURE_NAME             (feature)|#


#|typedef enum {
  GST_RANK_NONE                 = 0,
  GST_RANK_MARGINAL             = 64,
  GST_RANK_SECONDARY            = 128,
  GST_RANK_PRIMARY              = 256
} GstRank;|#

(define GST_RANK_NONE 0)
(define GST_RANK_MARGINAL 64)
(define GST_RANK_SECONDARY 128)
(define GST_RANK_PRIMARY 256)
  
;gboolean            (*GstPluginFeatureFilter)           (GstPluginFeature *feature, gpointer user_data);
(define GstPluginFeatureFilter (_cprocedure (list _GstPluginFeature-pointer _gpointer) _gboolean))

;gboolean            gst_plugin_feature_type_name_filter (GstPluginFeature *feature, GstTypeNameData *data);
(define-gstreamer gst_plugin_feature_type_name_filter (_fun _GstPluginFeature-pointer _GstTypeNameData-pointer -> _gboolean))

;void                gst_plugin_feature_set_rank         (GstPluginFeature *feature, guint rank);
(define-gstreamer gst_plugin_feature_set_rank (_fun _GstPluginFeature-pointer _guint -> _void))

;void                gst_plugin_feature_set_name         (GstPluginFeature *feature, const gchar *name);
(define-gstreamer gst_plugin_feature_set_name (_fun _GstPluginFeature-pointer _string -> _void))

;guint               gst_plugin_feature_get_rank         (GstPluginFeature *feature);
(define-gstreamer gst_plugin_feature_get_rank (_fun _GstPluginFeature-pointer -> _guint))

;const gchar *       gst_plugin_feature_get_name         (GstPluginFeature *feature);
(define-gstreamer gst_plugin_feature_get_name (_fun _GstPluginFeature-pointer -> _string))

;GstPluginFeature *  gst_plugin_feature_load             (GstPluginFeature *feature);
(define-gstreamer gst_plugin_feature_load (_fun _GstPluginFeature-pointer -> _GstPluginFeature-pointer))

;GList *             gst_plugin_feature_list_copy        (GList *list);
(define-gstreamer gst_plugin_feature_list_copy (_fun _GList-pointer -> _GList-pointer))

;void                gst_plugin_feature_list_free        (GList *list);
(define-gstreamer gst_plugin_feature_list_free (_fun _GList-pointer -> _void))

;gboolean            gst_plugin_feature_check_version    (GstPluginFeature *feature, guint min_major, guint min_minor, guint min_micro);
(define-gstreamer gst_plugin_feature_check_version (_fun _GstPluginFeature-pointer _guint _guint _guint -> _gboolean))
