#lang racket

(require "gst_base.rkt"
         "GstPlugin-ffi.rkt"
         "GstPluginFeature-ffi.rkt")

(provide (all-defined-out))


;;typedef struct _GstRegistry GstRegistry;
(define-cpointer-type _GstRegistry-pointer)

;GstRegistry *       gst_registry_get_default            (void);
(define-gstreamer gst_registry_get_default (_fun -> _GstRegistry-pointer))

;GList *             gst_registry_get_feature_list       (GstRegistry *registry, GType type);
(define-gstreamer gst_registry_get_feature_list (_fun _GstRegistry-pointer _GType -> _GList-pointer))

;guint32             gst_registry_get_feature_list_cookie (GstRegistry *registry);
(define-gstreamer gst_registry_get_feature_list_cookie (_fun _GstRegistry-pointer -> _guint32))

;GList *             gst_registry_get_feature_list_by_plugin (GstRegistry *registry, const gchar *name);
(define-gstreamer gst_registry_get_feature_list_by_plugin (_fun _GstRegistry-pointer _string -> _GList-pointer))

;;GstRegistry* -> GList*
(define-gstreamer*
  (_fun _GstRegistry-pointer -> _GList-pointer)
  gst_registry_get_path_list gst_registry_get_plugin_list)

;gboolean            gst_registry_add_plugin             (GstRegistry *registry, GstPlugin *plugin);
(define-gstreamer gst_registry_add_plugin (_fun _GstRegistry-pointer _GstPlugin-pointer -> _gboolean))

;void                gst_registry_remove_plugin          (GstRegistry *registry, GstPlugin *plugin);
(define-gstreamer gst_registry_remove_plugin (_fun _GstRegistry-pointer _GstPlugin-pointer -> _void))

;GList*              gst_registry_plugin_filter          (GstRegistry *registry, GstPluginFilter filter, gboolean first, gpointer user_data);
(define-gstreamer gst_registry_plugin_filter (_fun _GstRegistry-pointer GstPluginFilter _gboolean _gpointer -> _GList-pointer))

;GList*              gst_registry_feature_filter         (GstRegistry *registry, GstPluginFeatureFilter filter, gboolean first, gpointer user_data);
(define-gstreamer gst_registry_feature_filter (_fun _GstRegistry-pointer GstPluginFeatureFilter _gboolean _gpointer -> _GList-pointer))

;GstPlugin*          gst_registry_find_plugin            (GstRegistry *registry, const gchar *name);
(define-gstreamer gst_registry_find_plugin (_fun _GstRegistry-pointer _string -> _GstPlugin-pointer))

;GstPluginFeature*   gst_registry_find_feature           (GstRegistry *registry, const gchar *name, GType type);
(define-gstreamer gst_registry_find_feature (_fun _GstRegistry-pointer _string _GType -> _GstPluginFeature-pointer))

;GstPluginFeature *  gst_registry_lookup_feature         (GstRegistry *registry, const char *name);
(define-gstreamer gst_registry_lookup_feature (_fun _GstRegistry-pointer _string -> _GstPluginFeature-pointer))

;void                gst_registry_add_path               (GstRegistry *registry, const gchar *path);
(define-gstreamer gst_registry_add_path (_fun _GstRegistry-pointer _string -> _void))

;gboolean            gst_registry_scan_path              (GstRegistry *registry, const gchar *path);
(define-gstreamer gst_registry_scan_path (_fun _GstRegistry-pointer _string -> _gboolean))

;GstPlugin *         gst_registry_lookup                 (GstRegistry *registry, const char *filename);
(define-gstreamer  gst_registry_lookup (_fun _GstRegistry-pointer _string -> _GstPlugin-pointer))

;void                gst_registry_remove_feature         (GstRegistry *registry, GstPluginFeature *feature);
(define-gstreamer  gst_registry_remove_feature (_fun _GstRegistry-pointer _GstPluginFeature-pointer -> _void))

;gboolean            gst_registry_add_feature            (GstRegistry *registry, GstPluginFeature *feature);
(define-gstreamer  gst_registry_add_feature (_fun _GstRegistry-pointer _GstPluginFeature-pointer -> _gboolean))

;gboolean            gst_default_registry_check_feature_version (const gchar *feature_name, guint min_major, guint min_minor, guint min_micro);
(define-gstreamer  gst_default_registry_check_feature_version (_fun _string _guint _guint _guint -> _gboolean))
                                                        
#|#define             gst_default_registry_get_path_list
#define             gst_default_registry_add_plugin     (plugin)
#define             gst_default_registry_add_path       (path)
#define             gst_default_registry_find_plugin    (name)
#define             gst_default_registry_find_feature   (name,
                                                         type)
#define             gst_default_registry_get_plugin_list
#define             gst_default_registry_get_feature_list_cookie
#define             gst_default_registry_feature_filter (filter, first, user_data)|#
