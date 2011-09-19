#lang racket

(require ffi/unsafe)
(require "gst_base.rkt"
         "gst-structs-ffi.rkt"
         "GstClock-ffi.rkt"
         "GstObject-ffi.rkt"
         "GstMessage-ffi.rkt")

(provide (all-defined-out))


#|typedef enum {
  GST_BUS_FLUSHING      = (GST_OBJECT_FLAG_LAST << 0),
  /* padding */
  GST_BUS_FLAG_LAST     = (GST_OBJECT_FLAG_LAST << 1)
} GstBusFlags;|#

(define GST_BUS_FLUSHING (arithmetic-shift GST_OBJECT_FLAG_LAST 0))
(define GST_BUS_FLAG_LAST (arithmetic-shift GST_OBJECT_FLAG_LAST 1))

#|typedef enum
{
  GST_BUS_DROP = 0,
  GST_BUS_PASS = 1,
  GST_BUS_ASYNC = 2
} GstBusSyncReply;|#

(define GST_BUS_DROP 0)
(define GST_BUS_PASS 1)
(define GST_BUS_ASYNC 2)


;gboolean            (*GstBusFunc)                       (GstBus *bus, GstMessage *message, gpointer data); 
(define GstBusFunc (_cprocedure (list _GstBus-pointer _GstMessage-pointer _gpointer) _gboolean))

;GstBusSyncReply     (*GstBusSyncHandler)                (GstBus *bus, GstMessage *message, gpointer data); 
(define GstBusSyncHandler (_cprocedure (list _GstBus-pointer _GstMessage-pointer _gpointer) _int))

;GstBus*             gst_bus_new                         (void);
(define-gstreamer gst_bus_new (_fun -> _GstBus-pointer))

;gboolean            gst_bus_post                        (GstBus *bus, GstMessage *message);
(define-gstreamer gst_bus_post (_fun _GstBus-pointer _GstMessage-pointer -> _gboolean))

;gboolean            gst_bus_have_pending                (GstBus *bus);
(define-gstreamer gst_bus_have_pending (_fun _GstBus-pointer -> _gboolean))

;;GstBus* -> GstMessage*
(define-gstreamer*
  (_fun _GstBus-pointer -> _GstMessage-pointer)
  gst_bus_peek gst_bus_pop)

;GstMessage *        gst_bus_pop_filtered                (GstBus *bus, GstMessageType types);
(define-gstreamer gst_bus_pop_filtered (_fun _GstBus-pointer _GstMessageType -> _GstMessage-pointer))

;GstMessage *        gst_bus_timed_pop                   (GstBus *bus, GstClockTime timeout);
(define-gstreamer gst_bus_timed_pop (_fun _GstBus-pointer _GstClockTime -> (_or-null _GstMessage-pointer)))

;GstMessage *        gst_bus_timed_pop_filtered          (GstBus *bus, GstClockTime timeout, GstMessageType types);
(define-gstreamer gst_bus_timed_pop_filtered (_fun _GstBus-pointer _GstClockTime _GstMessageType -> _GstMessage-pointer))

;void                gst_bus_set_flushing                (GstBus *bus, gboolean flushing);
(define-gstreamer gst_bus_set_flushing (_fun _GstBus-pointer _gboolean -> _void))

;void                gst_bus_set_sync_handler            (GstBus *bus, GstBusSyncHandler func, gpointer data);
(define-gstreamer gst_bus_set_sync_handler (_fun _GstBus-pointer GstBusSyncHandler _gpointer -> _void))

;GstBusSyncReply     gst_bus_sync_signal_handler         (GstBus *bus, GstMessage *message, gpointer data);
(define-gstreamer gst_bus_sync_signal_handler (_fun _GstBus-pointer _GstMessage-pointer _gpointer -> _int))

;GSource *           gst_bus_create_watch                (GstBus *bus);
(define-gstreamer gst_bus_create_watch (_fun _GstBus-pointer -> _GSource-pointer))

;guint               gst_bus_add_watch_full              (GstBus *bus, gint priority, GstBusFunc func, gpointer user_data, GDestroyNotify notify);
(define-gstreamer gst_bus_add_watch_full (_fun _GstBus-pointer _gint GstBusFunc _gpointer GDestroyNotify -> _guint))

;guint               gst_bus_add_watch                   (GstBus *bus, GstBusFunc func, gpointer user_data);
(define-gstreamer gst_bus_add_watch (_fun _GstBus-pointer GstBusFunc _gpointer -> _guint))

;;GstBus* -> void
(define-gstreamer*
  (_fun _GstBus-pointer -> _void)
  gst_bus_disable_sync_message_emission 
  gst_bus_enable_sync_message_emission 
  gst_bus_add_signal_watch 
  gst_bus_remove_signal_watch)

;gboolean            gst_bus_async_signal_func           (GstBus *bus, GstMessage *message, gpointer data);
(define-gstreamer gst_bus_async_signal_func (_fun _GstBus-pointer _GstMessage-pointer _gpointer -> _gboolean))

;void                gst_bus_add_signal_watch_full       (GstBus *bus, gint priority);
(define-gstreamer gst_bus_add_signal_watch_full (_fun _GstBus-pointer _gint -> _void))

;GstMessage*         gst_bus_poll                        (GstBus *bus, GstMessageType events, GstClockTimeDiff timeout);
(define-gstreamer gst_bus_poll (_fun _GstBus-pointer _GstMessageType _GstClockTimeDiff -> (_or-null _GstMessage-pointer)))

