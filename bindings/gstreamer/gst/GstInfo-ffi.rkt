#lang racket

(require "gst_base.rkt")

(provide (all-defined-out))


;typedef struct _GstDebugMessage GstDebugMessage;
(define-cpointer-type _GstDebugMessage-pointer)

#|typedef enum {
  GST_LEVEL_NONE = 0,
  GST_LEVEL_ERROR,
  GST_LEVEL_WARNING,
  GST_LEVEL_INFO,
  GST_LEVEL_DEBUG,
  GST_LEVEL_LOG,
  GST_LEVEL_FIXME = 6,
  GST_LEVEL_TRACE = 7,
  /* add more */
  GST_LEVEL_MEMDUMP = 9,
  /* add more */
  GST_LEVEL_COUNT
} GstDebugLevel;|#


(define _GstDebugLevel
  (_enum '(GST_LEVEL_NONE = 0 GST_LEVEL_ERROR GST_LEVEL_WARNING GST_LEVEL_INFO GST_LEVEL_DEBUG GST_LEVEL_LOG GST_LEVEL_FIXME = 6 GST_LEVEL_TRACE = 7 GST_LEVEL_MEMDUMP = 9 GST_LEVEL_COUNT)))


#|typedef enum {
  /* colors */
  GST_DEBUG_FG_BLACK		= 0x0000,
  GST_DEBUG_FG_RED		= 0x0001,
  GST_DEBUG_FG_GREEN		= 0x0002,
  GST_DEBUG_FG_YELLOW		= 0x0003,
  GST_DEBUG_FG_BLUE		= 0x0004,
  GST_DEBUG_FG_MAGENTA		= 0x0005,
  GST_DEBUG_FG_CYAN		= 0x0006,
  GST_DEBUG_FG_WHITE		= 0x0007,
  /* background colors */
  GST_DEBUG_BG_BLACK		= 0x0000,
  GST_DEBUG_BG_RED		= 0x0010,
  GST_DEBUG_BG_GREEN		= 0x0020,
  GST_DEBUG_BG_YELLOW		= 0x0030,
  GST_DEBUG_BG_BLUE		= 0x0040,
  GST_DEBUG_BG_MAGENTA		= 0x0050,
  GST_DEBUG_BG_CYAN		= 0x0060,
  GST_DEBUG_BG_WHITE		= 0x0070,
  /* other formats */
  GST_DEBUG_BOLD		= 0x0100,
  GST_DEBUG_UNDERLINE		= 0x0200
} GstDebugColorFlags;|#

(define _GstDebugColorFlags
  (_enum '(GST_DEBUG_FG_BLACK = #x0000 GST_DEBUG_FG_RED = #x0001 GST_DEBUG_FG_GREEN = #x0002 GST_DEBUG_FG_YELLOW = #x0003 GST_DEBUG_FG_BLUE = #x0004 GST_DEBUG_FG_MAGENTA = #x0005 GST_DEBUG_FG_CYAN = #x0006 GST_DEBUG_FG_WHITE = #x0007 GST_DEBUG_BG_BLACK = #x0000 GST_DEBUG_BG_RED = #x0010 GST_DEBUG_BG_GREEN = #x0020 GST_DEBUG_BG_YELLOW = #x0030 GST_DEBUG_BG_BLUE = #x0040 GST_DEBUG_BG_MAGENTA	= #x0050 GST_DEBUG_BG_CYAN = #x0060 GST_DEBUG_BG_WHITE = #x0070 GST_DEBUG_BOLD = #x0100 GST_DEBUG_UNDERLINE = #x0200)))


#|typedef enum {
  GST_DEBUG_GRAPH_SHOW_MEDIA_TYPE         = (1<<0),
  GST_DEBUG_GRAPH_SHOW_CAPS_DETAILS       = (1<<1),
  GST_DEBUG_GRAPH_SHOW_NON_DEFAULT_PARAMS = (1<<2),
  GST_DEBUG_GRAPH_SHOW_STATES             = (1<<3),
  GST_DEBUG_GRAPH_SHOW_ALL                = ((1<<4)-1)
} GstDebugGraphDetails;|#

(define GST_DEBUG_GRAPH_SHOW_MEDIA_TYPE (arithmetic-shift 1 0))
(define GST_DEBUG_GRAPH_SHOW_CAPS_DETAILS (arithmetic-shift 1 1))
(define GST_DEBUG_GRAPH_SHOW_NON_DEFAULT_PARAMS (arithmetic-shift 1 2))
(define GST_DEBUG_GRAPH_SHOW_STATES (arithmetic-shift 1 3))
(define GST_DEBUG_GRAPH_SHOW_ALL (- (arithmetic-shift 1 4) 1))
  

#|typedef struct {
} GstDebugCategory;|#

(define-cpointer-type _GstDebugCategory-pointer)


#|#define             GST_LEVEL_DEFAULT
#define             GST_STR_NULL                        (str)
#define             GST_DEBUG_PAD_NAME                  (pad)
#define             GST_FUNCTION
#define             GST_DEBUG_CATEGORY                  (cat)
#define             GST_DEBUG_CATEGORY_EXTERN           (cat)
#define             GST_DEBUG_CATEGORY_STATIC           (cat)
#define             GST_DEBUG_CATEGORY_INIT             (cat, name, color, description)
#define             GST_DEBUG_CATEGORY_GET              (cat, name)
#define             GST_CAT_LEVEL_LOG                   (cat, level, object, ...)
#define             GST_CAT_ERROR_OBJECT                (cat, obj, ...)
#define             GST_CAT_WARNING_OBJECT              (cat, obj, ...)
#define             GST_CAT_INFO_OBJECT                 (cat, obj, ...)
#define             GST_CAT_DEBUG_OBJECT                (cat, obj, ...)
#define             GST_CAT_LOG_OBJECT                  (cat, obj, ...)
#define             GST_CAT_FIXME_OBJECT                (cat, obj, ...)
#define             GST_CAT_TRACE_OBJECT                (cat, obj, ...)
#define             GST_CAT_MEMDUMP_OBJECT              (cat, obj, msg, data, length)
#define             GST_CAT_ERROR                       (cat,  ...)
#define             GST_CAT_WARNING                     (cat, ...)
#define             GST_CAT_INFO                        (cat, ...)
#define             GST_CAT_DEBUG                       (cat, ...)
#define             GST_CAT_LOG                         (cat, ...)
#define             GST_CAT_FIXME                       (cat, ...)
#define             GST_CAT_TRACE                       (cat, ...)
#define             GST_CAT_MEMDUMP                     (cat, msg, data, length)
#define             GST_ERROR_OBJECT                    (obj, ...)
#define             GST_WARNING_OBJECT                  (obj, ...)
#define             GST_INFO_OBJECT                     (obj, ...)
#define             GST_DEBUG_OBJECT                    (obj, ...)
#define             GST_LOG_OBJECT                      (obj, ...)
#define             GST_FIXME_OBJECT                    (obj, ...)
#define             GST_TRACE_OBJECT                    (obj, ...)
#define             GST_MEMDUMP_OBJECT                  (obj, msg, data, length)
#define             GST_ERROR                           (...)
#define             GST_WARNING                         (...)
#define             GST_INFO                            (...)
#define             GST_DEBUG                           (...)
#define             GST_LOG                             (...)
#define             GST_FIXME                           (...)
#define             GST_TRACE                           (...)
#define             GST_MEMDUMP                         (msg, data, length)
#define             GST_DEBUG_REGISTER_FUNCPTR          (ptr)
#define             GST_DEBUG_FUNCPTR                   (ptr)
#define             GST_DEBUG_FUNCPTR_NAME              (ptr)
#define             GST_DEBUG_BIN_TO_DOT_FILE           (bin, details, file_name)
#define             GST_DEBUG_BIN_TO_DOT_FILE_WITH_TS   (bin, details, file_name)
#define             GST_TIME_FORMAT
#define             GST_TIME_ARGS                       (t)|#


;void (*GstLogFunction) (GstDebugCategory *category, GstDebugLevel level, const gchar *file, const gchar *function, gint line, GObject *object, GstDebugMessage *message, gpointer data);
(define GstLogFunction (_cprocedure '(_GstDebugCategory-pointer _GstDebugLevel _string _string _gint _GObject-pointer _GstDebugMessage-pointer _gpointer) _void))

;void gst_debug_log (GstDebugCategory *category, GstDebugLevel level, const gchar *file, const gchar *function, gint line, GObject *object, const gchar *format, ...);
(define-gstreamer gst_debug_log (_fun _GstDebugCategory-pointer _GstDebugLevel _string _string _gint _GObject-pointer _string (_list i _string) -> _void))

;void gst_debug_log_valist (GstDebugCategory *category, GstDebugLevel level, const gchar *file, const gchar *function, gint line,GObject *object, const gchar *format, va_list args);
(define-gstreamer gst_debug_log_valist (_fun _GstDebugCategory-pointer _GstDebugLevel _string _string _gint  _GObject-pointer _string (_list i _string) -> _void))

;const gchar *       gst_debug_message_get               (GstDebugMessage *message);
(define-gstreamer gst_debug_message_get (_fun _GstDebugMessage-pointer -> _string))

;void                gst_debug_log_default               (GstDebugCategory *category, GstDebugLevel level, const gchar *file, const gchar *function, gint line, GObject *object, GstDebugMessage *message, gpointer unused);
(define-gstreamer gst_debug_log_default (_fun _GstDebugCategory-pointer _GstDebugLevel _string _string _gint _GObject-pointer _GstDebugMessage-pointer _gpointer -> _void))

;const gchar *       gst_debug_level_get_name            (GstDebugLevel level);
(define-gstreamer gst_debug_level_get_name (_fun _GstDebugLevel -> _string))

;void                gst_debug_add_log_function          (GstLogFunction func, gpointer data);
(define-gstreamer gst_debug_add_log_function (_fun GstLogFunction _gpointer -> _void))

;guint               gst_debug_remove_log_function       (GstLogFunction func);
(define-gstreamer gst_debug_remove_log_function (_fun GstLogFunction -> _guint))

;guint               gst_debug_remove_log_function_by_data (gpointer data);
(define-gstreamer gst_debug_remove_log_function_by_data (_fun _gpointer -> _guint))

;;gboolean -> void
(define-gstreamer*
  (_fun _gboolean -> _void)
  gst_debug_set_active gst_debug_set_colored)

;;void -> gboolean
(define-gstreamer*
  (_fun -> _gboolean)
  gst_debug_is_active gst_debug_is_colored)

;void                gst_debug_set_default_threshold     (GstDebugLevel level);
(define-gstreamer gst_debug_set_default_threshold (_fun _GstDebugLevel -> _void))

;GstDebugLevel       gst_debug_get_default_threshold     (void);
(define-gstreamer gst_debug_get_default_threshold (_fun -> _GstDebugLevel))

;void                gst_debug_set_threshold_for_name    (const gchar *name, GstDebugLevel level);
(define-gstreamer gst_debug_set_threshold_for_name (_fun _string _GstDebugLevel -> _void))

;void                gst_debug_unset_threshold_for_name  (const gchar *name);
(define-gstreamer gst_debug_unset_threshold_for_name (_fun _string -> _void))

;;GstDebugCategory* -> void
(define-gstreamer*
  (_fun _GstDebugCategory-pointer -> _void)
  gst_debug_category_free gst_debug_category_reset_threshold)

;void                gst_debug_category_set_threshold    (GstDebugCategory *category, GstDebugLevel level);
(define-gstreamer gst_debug_category_set_threshold (_fun _GstDebugCategory-pointer _GstDebugLevel -> _void))

;GstDebugLevel       gst_debug_category_get_threshold    (GstDebugCategory *category);
(define-gstreamer gst_debug_category_get_threshold (_fun _GstDebugCategory-pointer -> _GstDebugLevel))

;;GstDebugCategory* -> gchar*
(define-gstreamer*
  (_fun _GstDebugCategory-pointer -> _string)
  gst_debug_category_get_name gst_debug_category_get_description)

;guint               gst_debug_category_get_color        (GstDebugCategory *category);
(define-gstreamer gst_debug_category_get_color (_fun _GstDebugCategory-pointer -> _guint))

;GSList *            gst_debug_get_all_categories        (void);
(define-gstreamer gst_debug_get_all_categories (_fun -> _GSList-pointer))

;gchar *             gst_debug_construct_term_color      (guint colorinfo);
(define-gstreamer gst_debug_construct_term_color (_fun _guint -> _string))

;gint                gst_debug_construct_win_color       (guint colorinfo);
(define-gstreamer gst_debug_construct_win_color (_fun _guint -> _gint))

;void                gst_debug_print_stack_trace         (void);
(define-gstreamer gst_debug_print_stack_trace (_fun -> _void))

