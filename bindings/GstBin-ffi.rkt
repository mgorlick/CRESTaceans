#lang racket

(require "gst_base.rkt"
         "GstStructs-ffi.rkt"
         "GstClock-ffi.rkt"
         "GstElement-ffi.rkt"
         "GstMessage-ffi.rkt"
         "GstPadTemplate-ffi.rkt")

(provide (all-defined-out))


;;typedef struct _GstBin GstBin;
;;struct _GstBin {
;;  GstElement	 element;
;;  /*< public >*/ /* with LOCK */
;;  gint		 numchildren;
;;  GList		*children;
;;  guint32	 children_cookie;
;;  GstBus        *child_bus;
;;  GList         *messages;
;;  gboolean	 polling;
;;  gboolean       state_dirty;
;;  gboolean       clock_dirty;
;;  GstClock	*provided_clock;
;;  GstElement    *clock_provider;
;;  /*< private >*/
;;  GstBinPrivate *priv;
;;  gpointer _gst_reserved[GST_PADDING - 1];
;;};

(define-cstruct _GstBin
  ([element _GstElement]
   [numchildren _gint]
   [children _GList-pointer]
   [children_cookie _guint32]
   [child_bus _GstBus-pointer]
   [messages _GList-pointer]
   [polling _gboolean]
   [state_dirty _gboolean]
   [clock_dirty _gboolean]
   [provided_clock _GstClock-pointer]
   [clock_provider _GstElement-pointer]))


;;typedef struct {
;;  GstElementClass parent_class;
;;  /* virtual methods for subclasses */
;;  gboolean (*add_element)		(GstBin *bin, GstElement *element);
;;  gboolean (*remove_element) (GstBin *bin, GstElement *element);
;;  void (*handle_message) (GstBin *bin, GstMessage *message);
;;} GstBinClass;

(define-cstruct _GstBinClass
  ([parent_class _GstElementClass]
   [add_element (_ptr io (_fun _GstBin-pointer _GstElement-pointer -> _gboolean))]
   [remove_element (_ptr io (_fun _GstBin-pointer _GstElement-pointer -> _gboolean))]
   [handle_message (_ptr io (_fun _GstBin-pointer _GstMessage-pointer -> _void))]))


#|typedef enum {
  /* padding */
  GST_BIN_FLAG_LAST		= (GST_ELEMENT_FLAG_LAST << 5)
} GstBinFlags;|#

(define GST_BIN_FLAG_LAST (arithmetic-shift GST_ELEMENT_FLAG_LAST 5))


;GstElement*         gst_bin_new                         (const gchar *name);
(define-gstreamer gst_bin_new (_fun _string -> _GstElement-pointer))

;GstBin* GstElement* -> gboolean
(define-gstreamer*
  (_fun _GstBin-pointer _GstElement-pointer -> _gboolean)
  gst_bin_add gst_bin_remove)

;GstBin* gchar* -> GstElement*
(define-gstreamer*
  (_fun _GstBin-pointer _string -> _GstElement-pointer)
  gst_bin_get_by_name gst_bin_get_by_name_recurse_up)

;GstElement*         gst_bin_get_by_interface            (GstBin *bin, GType iface);
(define-gstreamer gst_bin_get_by_interface (_fun _GstBin-pointer _GType -> _GstElement-pointer))

;GstBin* -> GstIterator*
(define-gstreamer*
  (_fun _GstBin-pointer -> _GstIterator-pointer)
  gst_bin_iterate_elements gst_bin_iterate_recurse gst_bin_iterate_sinks gst_bin_iterate_sorted gst_bin_iterate_sources)

;GstIterator*        gst_bin_iterate_all_by_interface    (GstBin *bin, GType iface);
(define-gstreamer gst_bin_iterate_all_by_interface (_fun _GstBin-pointer _GType -> _GstIterator-pointer))

;gboolean            gst_bin_recalculate_latency         (GstBin *bin);
(define-gstreamer gst_bin_recalculate_latency (_fun _GstBin-pointer -> _gboolean))

;void funcName(GstBin *bin, GstElement *element_1,...);
;GstBin* GstElement* ... -> void                          ;;;OJO
(define-gstreamer*
  (_fun _GstBin-pointer _GstElement-pointer (_list i _GstElement-pointer) -> _void)
  gst_bin_add_many gst_bin_remove_many)

;GstBin* GstPadDirection -> GstPad*
(define-gstreamer*
  (_fun _GstBin-pointer _GstPadDirection -> _GstPad-pointer)
  gst_bin_find_unlinked_pad gst_bin_find_unconnected_pad)

#|
#define             GST_BIN_CHILDREN                    (bin)
#define             GST_BIN_CHILDREN_COOKIE             (bin)
#define             GST_BIN_NUMCHILDREN                 (bin)
|#
