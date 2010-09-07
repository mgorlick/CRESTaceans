#lang racket

(require "gst_base.rkt"
         "GstStructs-ffi.rkt"
         "GstBuffer-ffi.rkt"
         "GstBufferList-ffi.rkt"
         "GstCaps-ffi.rkt"
         "GstEvent-ffi.rkt"
         "GstFormat-ffi.rkt"
         "GstObject-ffi.rkt"
         "GstPadTemplate-ffi.rkt"
         "GstStructure-ffi.rkt"
         "GstTask-ffi.rkt")
         
         
(provide (all-defined-out))

  
#|  typedef enum {
  GST_PAD_BLOCKED       = (GST_OBJECT_FLAG_LAST << 0),
  GST_PAD_FLUSHING      = (GST_OBJECT_FLAG_LAST << 1),
  GST_PAD_IN_GETCAPS    = (GST_OBJECT_FLAG_LAST << 2),
  GST_PAD_IN_SETCAPS    = (GST_OBJECT_FLAG_LAST << 3),
  GST_PAD_BLOCKING = (GST_OBJECT_FLAG_LAST << 4),
  /* padding */
  GST_PAD_FLAG_LAST     = (GST_OBJECT_FLAG_LAST << 8)
} GstPadFlags;|#

(define GST_PAD_BLOCKED (arithmetic-shift GST_OBJECT_FLAG_LAST 0))
(define GST_PAD_FLUSHING (arithmetic-shift GST_OBJECT_FLAG_LAST 1))
(define GST_PAD_IN_GETCAPS (arithmetic-shift GST_OBJECT_FLAG_LAST 2))
(define GST_PAD_IN_SETCAPS (arithmetic-shift GST_OBJECT_FLAG_LAST 3))
(define GST_PAD_BLOCKING (arithmetic-shift GST_OBJECT_FLAG_LAST 4))
(define GST_PAD_FLAG_LAST (arithmetic-shift GST_OBJECT_FLAG_LAST 8))

  
#|#define             GST_PAD_LINK_FAILED                 (ret)
#define             GST_PAD_LINK_SUCCESSFUL             (ret)
#define GST_PAD_LINK_CHECK_DEFAULT (GST_PAD_LINK_CHECK_HIERARCHY | GST_PAD_LINK_CHECK_CAPS)
#define gst_pad_get_name(pad) gst_object_get_name (GST_OBJECT_CAST (pad))
#define             gst_pad_get_parent                  (pad)
#define             GST_PAD_CAPS                        (pad)
#define             GST_PAD_GET_STREAM_LOCK             (pad)
#define             GST_PAD_STREAM_LOCK                 (pad)
#define             GST_PAD_STREAM_LOCK_FULL            (pad, t)
#define             GST_PAD_STREAM_TRYLOCK              (pad)
#define             GST_PAD_STREAM_UNLOCK               (pad)
#define             GST_PAD_STREAM_UNLOCK_FULL          (pad)
#define             GST_FLOW_IS_FATAL                   (ret)
#define             GST_FLOW_IS_SUCCESS                 (ret)|#

  
#|  typedef enum {
  GST_PAD_LINK_CHECK_NOTHING       = 0,
  GST_PAD_LINK_CHECK_HIERARCHY     = 1 << 0,
  GST_PAD_LINK_CHECK_TEMPLATE_CAPS = 1 << 1,
  GST_PAD_LINK_CHECK_CAPS          = 1 << 2,
} GstPadLinkCheck;|#

  
(define GST_PAD_LINK_CHECK_NOTHING 0)
(define GST_PAD_LINK_CHECK_HIERARCHY (arithmetic-shift 1 0))
(define GST_PAD_LINK_CHECK_TEMPLATE_CAPS (arithmetic-shift 1 1))
(define GST_PAD_LINK_CHECK_CAPS (arithmetic-shift 1 2))


  
;GstPadDirection     gst_pad_get_direction               (GstPad *pad);
(define-gstreamer gst_pad_get_direction (_fun _GstPad-pointer -> _GstPadDirection))

;GstElement*         gst_pad_get_parent_element          (GstPad *pad);
(define-gstreamer gst_pad_get_parent_element (_fun _GstPad-pointer -> _GstElement-pointer))

;GstPadTemplate*     gst_pad_get_pad_template            (GstPad *pad);
(define-gstreamer gst_pad_get_pad_template (_fun _GstPad-pointer -> _GstPadTemplate-pointer))

;GstPadLinkReturn    gst_pad_link                        (GstPad *srcpad, GstPad *sinkpad);
(define-gstreamer gst_pad_link (_fun _GstPad-pointer _GstPad-pointer -> _GstPadLinkReturn))

;GstPadLinkReturn    gst_pad_link_full                   (GstPad *srcpad, GstPad *sinkpad, GstPadLinkCheck flags);
;(define-gstreamer gst_pad_link_full (_fun _GstPad-pointer _GstPad-pointer _GstPadLinkCheck -> _GstPadLinkReturn))  ;;NOT IN LIB


;;GstPad* GstPad* -> gboolean
(define-gstreamer*
  (_fun _GstPad-pointer _GstPad-pointer -> _gboolean)
  gst_pad_unlink gst_pad_can_link)

;;GstPad* GstCaps* -> gboolean
(define-gstreamer*
  (_fun _GstPad-pointer _GstCaps-pointer -> _gboolean)
  gst_pad_set_caps gst_pad_accept_caps gst_pad_peer_accept_caps gst_pad_proxy_setcaps)

;;GstPad* -> gboolean
(define-gstreamer*
  (_fun _GstPad-pointer -> _gboolean)
  gst_pad_is_linked gst_pad_is_active gst_pad_is_blocked gst_pad_is_blocking gst_pad_check_pull_range gst_pad_pause_task gst_pad_stop_task)

;;GstPad* -> GstCaps*
(define-gstreamer*
  (_fun _GstPad-pointer -> _GstCaps-pointer)
  gst_pad_get_caps gst_pad_get_caps_reffed gst_pad_get_allowed_caps gst_pad_get_negotiated_caps gst_pad_get_pad_template_caps gst_pad_peer_get_caps gst_pad_peer_get_caps_reffed gst_pad_proxy_getcaps gst_pad_get_fixed_caps_func)

;void                gst_pad_use_fixed_caps              (GstPad *pad);
(define-gstreamer gst_pad_use_fixed_caps (_fun _GstPad-pointer -> _void))

;GstPad*              gst_pad_get_peer                    (GstPad *pad);
(define-gstreamer gst_pad_get_peer (_fun _GstPad-pointer -> _GstPad-pointer))

;;GstPad* gboolean -> gboolean
(define-gstreamer*
  (_fun _GstPad-pointer _gboolean -> _gboolean)
  gst_pad_set_blocked gst_pad_activate_pull gst_pad_activate_push gst_pad_set_active)

;gboolean            gst_pad_set_blocked_async           (GstPad *pad, gboolean blocked, GstPadBlockCallback callback, gpointer user_data);
(define-gstreamer gst_pad_set_blocked_async (_fun _GstPad-pointer _gboolean GstPadBlockCallback _gpointer -> _gboolean))

;gboolean            gst_pad_set_blocked_async_full      (GstPad *pad, gboolean blocked, GstPadBlockCallback callback, gpointer user_data, GDestroyNotify destroy_data);
(define-gstreamer gst_pad_set_blocked_async_full (_fun _GstPad-pointer _gboolean GstPadBlockCallback _gpointer _GDestroyNotify -> _gboolean))

;;GstPad* GCallback gpointer -> gulong
(define-gstreamer*
  (_fun _GstPad-pointer _GCallback-pointer _gpointer -> _gulong)
  gst_pad_add_data_probe gst_pad_add_buffer_probe gst_pad_add_event_probe)

;;GstPad* GCallback gpointer GDestroyNotify -> gulong
(define-gstreamer*
  (_fun _GstPad-pointer _GCallback-pointer _gpointer _GDestroyNotify -> _gulong)
  gst_pad_add_data_probe_full gst_pad_add_buffer_probe_full gst_pad_add_event_probe_full)

;;GstPad* guint -> void
(define-gstreamer*
  (_fun _GstPad-pointer _guint -> _void)
  gst_pad_remove_data_probe gst_pad_remove_buffer_probe gst_pad_remove_event_probe)

;GstPad*             gst_pad_new                         (const gchar *name, GstPadDirection direction);
(define-gstreamer gst_pad_new (_fun _string _GstPadDirection -> _GstPad-pointer))

;GstPad*             gst_pad_new_from_template           (GstPadTemplate *templ, const gchar *name);
(define-gstreamer gst_pad_new_from_template (_fun _GstPadTemplate-pointer _string -> _GstPad-pointer))

;GstPad*             gst_pad_new_from_static_template    (GstStaticPadTemplate *templ, const gchar *name);
(define-gstreamer gst_pad_new_from_static_template (_fun _GstStaticPadTemplate-pointer _string -> _GstPad-pointer))

;;GstPad* guint64 gint GstCaps* GstBuffer** -> GstFlowReturn
(define-gstreamer*
  (_fun _GstPad-pointer _guint64 _gint _GstCaps-pointer (_ptr io (_ptr io _GstBuffer)) -> _GstFlowReturn)
  gst_pad_alloc_buffer gst_pad_alloc_buffer_and_set_caps)

;void                gst_pad_set_bufferalloc_function    (GstPad *pad, GstPadBufferAllocFunction bufalloc);
(define-gstreamer gst_pad_set_bufferalloc_function (_fun _GstPad-pointer GstPadBufferAllocFunction -> _void))

;void                gst_pad_set_chain_function          (GstPad *pad, GstPadChainFunction chain);
(define-gstreamer gst_pad_set_chain_function (_fun _GstPad-pointer GstPadChainFunction -> _void))

;GstFlowReturn       (*GstPadChainListFunction)          (GstPad *pad, GstBufferList *list);
(define GstPadChainListFunction (_cprocedure '(_GstPad-pointer _GstBufferList-pointer) _GstFlowReturn))

;void                gst_pad_set_chain_list_function     (GstPad *pad, GstPadChainListFunction chainlist);
(define-gstreamer gst_pad_set_chain_list_function (_fun _GstPad-pointer GstPadChainListFunction -> _void))

;void                gst_pad_set_checkgetrange_function  (GstPad *pad, GstPadCheckGetRangeFunction check);
(define-gstreamer gst_pad_set_checkgetrange_function (_fun _GstPad-pointer GstPadCheckGetRangeFunction -> _void))

;GstFlowReturn       gst_pad_get_range                   (GstPad *pad, guint64 offset, guint size, GstBuffer **buffer);
(define-gstreamer gst_pad_get_range (_fun _GstPad-pointer _guint64 _guint (_ptr io _GstBuffer-pointer) -> _GstFlowReturn))

;void                gst_pad_set_getrange_function       (GstPad *pad, GstPadGetRangeFunction get);
(define-gstreamer gst_pad_set_getrange_function (_fun _GstPad-pointer GstPadGetRangeFunction -> _void))

;void                gst_pad_set_event_function          (GstPad *pad, GstPadEventFunction event);
(define-gstreamer gst_pad_set_event_function (_fun _GstPad-pointer GstPadEventFunction -> _void))

;void                gst_pad_set_link_function           (GstPad *pad, GstPadLinkFunction link);
(define-gstreamer gst_pad_set_link_function (_fun _GstPad-pointer GstPadLinkFunction -> _void))

;void                gst_pad_set_unlink_function         (GstPad *pad, GstPadUnlinkFunction unlink);
(define-gstreamer gst_pad_set_unlink_function (_fun _GstPad-pointer GstPadUnlinkFunction -> _void))

;void                gst_pad_set_acceptcaps_function     (GstPad *pad, GstPadAcceptCapsFunction acceptcaps);
(define-gstreamer gst_pad_set_acceptcaps_function (_fun _GstPad-pointer GstPadAcceptCapsFunction -> _void))

;void                gst_pad_set_getcaps_function        (GstPad *pad, GstPadGetCapsFunction getcaps);
(define-gstreamer gst_pad_set_getcaps_function (_fun _GstPad-pointer GstPadGetCapsFunction -> _void))

;void                gst_pad_set_setcaps_function        (GstPad *pad, GstPadSetCapsFunction setcaps);
(define-gstreamer gst_pad_set_setcaps_function (_fun _GstPad-pointer GstPadSetCapsFunction -> _void))

;void                gst_pad_fixate_caps                 (GstPad *pad, GstCaps *caps);
(define-gstreamer gst_pad_fixate_caps (_fun _GstPad-pointer _GstCaps-pointer -> _void))

;void                gst_pad_set_fixatecaps_function     (GstPad *pad, GstPadFixateCapsFunction fixatecaps);
(define-gstreamer gst_pad_set_fixatecaps_function (_fun _GstPad-pointer GstPadFixateCapsFunction -> _void))

;void                gst_pad_set_activate_function       (GstPad *pad, GstPadActivateFunction activate);
(define-gstreamer gst_pad_set_activate_function (_fun _GstPad-pointer GstPadActivateFunction -> _void))

;;GstPad* GstPadActivateModeFunction -> void
(define-gstreamer*
  (_fun _GstPad-pointer GstPadActivateModeFunction -> _void)
  gst_pad_set_activatepush_function gst_pad_set_activatepull_function)



;GstFlowReturn       gst_pad_push                        (GstPad *pad, GstBuffer *buffer);
(define-gstreamer gst_pad_push (_fun _GstPad-pointer _GstBuffer-pointer -> _GstFlowReturn))

;;GstPad* GstEvent* -> gboolean
(define-gstreamer*
  (_fun _GstPad-pointer _GstEvent-pointer -> _gboolean)
  gst_pad_push_event gst_pad_send_event gst_pad_event_default)

;GstFlowReturn       gst_pad_push_list                   (GstPad *pad, GstBufferList *list);
(define-gstreamer gst_pad_push_list (_fun _GstPad-pointer _GstBufferList-pointer -> _GstFlowReturn))

;GstFlowReturn       gst_pad_pull_range                  (GstPad *pad, guint64 offset, guint size, GstBuffer **buffer);
(define-gstreamer gst_pad_pull_range (_fun _GstPad-pointer _guint64 _guint (_ptr io _GstBuffer-pointer) -> _GstFlowReturn))



;;GstPad* GstQuery* -> gboolean
(define-gstreamer*
  (_fun _GstPad-pointer _GstQuery-pointer -> _gboolean)
  gst_pad_query gst_pad_peer_query gst_pad_query_default)

;;GstPad* GstFormat* gint64* -> gboolean
(define-gstreamer*
  (_fun _GstPad-pointer (_ptr io _GstFormat) (_ptr io _gint64) -> _gboolean)
  gst_pad_query_position gst_pad_query_duration gst_pad_query_peer_position gst_pad_query_peer_duration)

;;GstPad* GstFormat gint64 GstFormat* gint64*-> gboolean
(define-gstreamer*
  (_fun _GstPad-pointer _GstFormat _gint64 (_ptr io _GstFormat) (_ptr io _gint64) -> _gboolean)
  gst_pad_query_convert gst_pad_query_peer_convert)

;void                gst_pad_set_query_function          (GstPad *pad, GstPadQueryFunction query);
(define-gstreamer gst_pad_set_query_function (_fun _GstPad-pointer GstPadQueryFunction -> _void))

;void                gst_pad_set_query_type_function     (GstPad *pad, GstPadQueryTypeFunction type_func);
(define-gstreamer gst_pad_set_query_type_function (_fun _GstPad-pointer GstPadQueryTypeFunction -> _void))


;;GstPad* -> GstQueryType*
(define-gstreamer*
  (_fun _GstPad-pointer -> (_ptr io _GstQueryType))
  gst_pad_get_query_types gst_pad_get_query_types_default)

;void                gst_pad_set_iterate_internal_links_function (GstPad *pad, GstPadIterIntLinkFunction iterintlink);
(define-gstreamer gst_pad_set_iterate_internal_links_function (_fun _GstPad-pointer GstPadIterIntLinkFunction -> _void))

;;GstPad* -> GstIterator*
(define-gstreamer*
  (_fun _GstPad-pointer -> _GstIterator-pointer)
  gst_pad_iterate_internal_links gst_pad_iterate_internal_links_default)

;void                gst_pad_load_and_link               (xmlNodePtr self, GstObject *parent);
;(define-gstreamer gst_pad_load_and_link (_fun _xmlNodePtr _GstObject-pointer -> _void))

;gboolean            (*GstPadDispatcherFunction)         (GstPad *pad, gpointer data);
(define GstPadDispatcherFunction (_cprocedure '(_GstPad-pointer _gpointer) _gboolean))

;gboolean            gst_pad_dispatcher                  (GstPad *pad, GstPadDispatcherFunction dispatch, gpointer data);
(define-gstreamer gst_pad_dispatcher (_fun _GstPad-pointer GstPadDispatcherFunction _gpointer -> _gboolean))

;void                gst_pad_set_element_private         (GstPad *pad, gpointer priv);
(define-gstreamer gst_pad_set_element_private (_fun _GstPad-pointer _gpointer -> _void))

;gpointer            gst_pad_get_element_private         (GstPad *pad);
(define-gstreamer gst_pad_get_element_private (_fun _GstPad-pointer -> _gpointer))

;GstFlowReturn       gst_pad_chain                       (GstPad *pad, GstBuffer *buffer);
(define-gstreamer gst_pad_chain (_fun _GstPad-pointer _GstBuffer-pointer -> _GstFlowReturn))

;GstFlowReturn       gst_pad_chain_list                  (GstPad *pad, GstBufferList *list);
(define-gstreamer gst_pad_chain_list (_fun _GstPad-pointer _GstBufferList-pointer -> _GstFlowReturn))

;gboolean            gst_pad_start_task                  (GstPad *pad, GstTaskFunction func, gpointer data);
(define-gstreamer gst_pad_start_task (_fun _GstPad-pointer GstTaskFunction _gpointer -> _gboolean))



