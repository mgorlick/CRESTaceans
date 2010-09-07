#lang racket

(require "gst_base.rkt"
         "GstStructure-ffi.rkt")

(provide (all-defined-out))

;typedef struct _GstPlugin GstPlugin;
(define-cpointer-type _GstPlugin-pointer)


;gboolean            (*GstPluginInitFunc)                (GstPlugin *plugin);
(define GstPluginInitFunc (_cprocedure '(_GstPlugin-pointer) _gboolean))

;gboolean            (*GstPluginInitFullFunc)            (GstPlugin *plugin, gpointer user_data);
(define GstPluginInitFullFunc (_cprocedure '(_GstPlugin-pointer _gpointer) _gboolean))

;gboolean            (*GstPluginFilter)                  (GstPlugin *plugin, gpointer user_data);
(define GstPluginFilter (_cprocedure '(_GstPlugin-pointer _gpointer) _gboolean))


#|typedef struct {
  gint major_version;
  gint minor_version;
  const gchar *name;
  const gchar *description;
  GstPluginInitFunc plugin_init;
  const gchar *version;
  const gchar *license;
  const gchar *source;
  const gchar *package;
  const gchar *origin;
  gpointer _gst_reserved[GST_PADDING]; ;;OJO!!
} GstPluginDesc;|#

(define-cstruct _GstPluginDesc
  ([major_version _gint]
   [minor_version _gint]
   [name _string]
   [description _string]
   [plugin_init GstPluginInitFunc]
   [version _string]
   [license _string]
   [source _string]
   [package _string]
   [origin _string]
   [_gst_reserved _gpointer]))



#|#define             GST_PLUGIN_ERROR
#define             GST_LICENSE_UNKNOWN
#define             GST_PLUGIN_DEFINE                   (major, minor, name, description, init, version, license, package, origin)
#define             GST_PLUGIN_DEFINE_STATIC            (major, minor, name, description, init, version, license, package, origin)|#


#|typedef enum
{
  GST_PLUGIN_ERROR_MODULE,
  GST_PLUGIN_ERROR_DEPENDENCIES,
  GST_PLUGIN_ERROR_NAME_MISMATCH
} GstPluginError;|#

(define _GstPluginError
  (_enum '(GST_PLUGIN_ERROR_MODULE GST_PLUGIN_ERROR_DEPENDENCIES GST_PLUGIN_ERROR_NAME_MISMATCH)))


;GQuark              gst_plugin_error_quark              (void);
(define-gstreamer gst_plugin_error_quark (_fun -> _GQuark))



;;GstPlugin* -> gchar*
(define-gstreamer*
  (_fun _GstPlugin-pointer -> _string)
  gst_plugin_get_name gst_plugin_get_description gst_plugin_get_filename gst_plugin_get_license gst_plugin_get_package gst_plugin_get_origin gst_plugin_get_source gst_plugin_get_version)

;GModule *           gst_plugin_get_module               (GstPlugin *plugin);
(define-gstreamer gst_plugin_get_module (_fun _GstPlugin-pointer -> _GModule-pointer))

;gboolean            gst_plugin_is_loaded                (GstPlugin *plugin);
(define-gstreamer gst_plugin_is_loaded (_fun _GstPlugin-pointer -> _gboolean))

;const GstStructure* gst_plugin_get_cache_data           (GstPlugin *plugin);
(define-gstreamer gst_plugin_get_cache_data (_fun _GstPlugin-pointer -> _GstStructure-pointer))

;void                gst_plugin_set_cache_data           (GstPlugin *plugin, GstStructure *cache_data);
(define-gstreamer gst_plugin_set_cache_data (_fun _GstPlugin-pointer _GstStructure-pointer -> _void))

;gboolean            gst_plugin_name_filter              (GstPlugin *plugin, const gchar *name);
(define-gstreamer gst_plugin_name_filter (_fun _GstPlugin-pointer _string -> _gboolean))

;GstPlugin *         gst_plugin_load_file                (const gchar *filename, GError **error);
(define-gstreamer gst_plugin_load_file (_fun _string (_ptr io _GError-pointer) -> _GstPlugin-pointer))

;GstPlugin *         gst_plugin_load                     (GstPlugin *plugin);
(define-gstreamer gst_plugin_load (_fun _GstPlugin-pointer -> _GstPlugin-pointer))

;GstPlugin *         gst_plugin_load_by_name             (const gchar *name);
(define-gstreamer gst_plugin_load_by_name (_fun _string -> _GstPlugin-pointer))

;void                gst_plugin_list_free                (GList *list);
(define-gstreamer gst_plugin_list_free (_fun _GList-pointer -> _void))

;gboolean gst_plugin_register_static (gint major_version, gint minor_version, const gchar *name, const gchar *description, GstPluginInitFunc init_func, const gchar *version, const gchar *license, const gchar *source, const gchar *package, const gchar *origin);
(define-gstreamer gst_plugin_register_static (_fun _gint _gint _string _string GstPluginInitFunc _string _string _string _string _string -> _gboolean))

;gboolean gst_plugin_register_static_full (gint major_version, gint minor_version, const gchar *name, const gchar *description, GstPluginInitFullFunc init_full_func, const gchar *version, const gchar *license, const gchar *source, const gchar *package, const gchar *origin, gpointer user_data);
(define-gstreamer gst_plugin_register_static_full (_fun _gint _gint _string _string GstPluginInitFullFunc _string _string _string _string _string _gpointer -> _gboolean))


#|typedef enum
{
  GST_PLUGIN_FLAG_CACHED = (1<<0),
  GST_PLUGIN_FLAG_BLACKLISTED = (1<<1)
} GstPluginFlags;|#

(define GST_PLUGIN_FLAG_CACHED (arithmetic-shift 1 0))
(define GST_PLUGIN_FLAG_BLACKLISTED (arithmetic-shift 1 1))

#|typedef enum {
  GST_PLUGIN_DEPENDENCY_FLAG_NONE = 0,
  GST_PLUGIN_DEPENDENCY_FLAG_RECURSE = (1 << 0),
  GST_PLUGIN_DEPENDENCY_FLAG_PATHS_ARE_DEFAULT_ONLY = (1 << 1),
  GST_PLUGIN_DEPENDENCY_FLAG_FILE_NAME_IS_SUFFIX = (1 << 2)
} GstPluginDependencyFlags;|#

(define GST_PLUGIN_DEPENDENCY_FLAG_NONE 0)
(define GST_PLUGIN_DEPENDENCY_FLAG_RECURSE (arithmetic-shift 1 0))
(define GST_PLUGIN_DEPENDENCY_FLAG_PATHS_ARE_DEFAULT_ONLY (arithmetic-shift 1 1))
(define GST_PLUGIN_DEPENDENCY_FLAG_FILE_NAME_IS_SUFFIX (arithmetic-shift 1 2))

;void                gst_plugin_add_dependency           (GstPlugin *plugin, const gchar **env_vars, const gchar **paths, const gchar **names, GstPluginDependencyFlags flags);
(define-gstreamer gst_plugin_add_dependency (_fun _GstPlugin-pointer (_ptr io _string) (_ptr io _string) (_ptr io _string) _int -> _void))

;void                gst_plugin_add_dependency_simple    (GstPlugin *plugin, const gchar *env_vars, const gchar *paths, const gchar *names, GstPluginDependencyFlags flags);
(define-gstreamer gst_plugin_add_dependency_simple (_fun _GstPlugin-pointer _string _string _string _int -> _void))
