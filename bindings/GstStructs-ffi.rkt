#lang racket

(require "gst_base.rkt"
         "GstCaps-ffi.rkt"
         "GstClock-ffi.rkt"
         "GstMiniObject-ffi.rkt"
         "GstPadTemplate-ffi.rkt"
         "GstStructure-ffi.rkt"
         "GstTask-ffi.rkt")

(provide (all-defined-out))


;GST BUS

;;typedef struct _GstBus GstBus;         ;;Opaque
(define-cpointer-type _GstBus-pointer)


;;GST ITERATOR

;;typedef struct {} GstIterator;

(define-cpointer-type _GstIterator-pointer)


;;GST ELEMENT

#|
typedef enum {
  GST_STATE_VOID_PENDING        = 0,
  GST_STATE_NULL                = 1,
  GST_STATE_READY               = 2,
  GST_STATE_PAUSED              = 3,
  GST_STATE_PLAYING             = 4
} GstState;
|#

(define GST_STATE_VOID_PENDING 0)
(define GST_STATE_NULL 1)
(define GST_STATE_READY 2)
(define GST_STATE_PAUSED 3)
(define GST_STATE_PLAYING 4)


#|typedef enum {
  GST_STATE_CHANGE_FAILURE             = 0,
  GST_STATE_CHANGE_SUCCESS             = 1,
  GST_STATE_CHANGE_ASYNC               = 2,
  GST_STATE_CHANGE_NO_PREROLL          = 3
} GstStateChangeReturn;|#

(define _GstStateChangeReturn
  (_enum '(GST_STATE_CHANGE_FAILURE = 0 GST_STATE_CHANGE_SUCCESS = 1 GST_STATE_CHANGE_ASYNC = 2 GST_STATE_CHANGE_NO_PREROLL = 3)))
  
#|
typedef struct {
  GStaticRecMutex      *state_lock;
  /* element state */
  GCond                *state_cond;
  guint32               state_cookie;
  GstState              current_state;
  GstState              next_state;
  GstState              pending_state;
  GstStateChangeReturn  last_return;
  GstBus               *bus;
  /* allocated clock */
  GstClock             *clock;
  GstClockTimeDiff      base_time; /* NULL/READY: 0 - PAUSED: current time - PLAYING: difference to clock */
  /* element pads, these lists can only be iterated while holding the LOCK or checking thne cookie after each LOCK. */
  guint16               numpads;
  GList                *pads;
  guint16               numsrcpads;
  GList                *srcpads;
  guint16               numsinkpads;
  GList                *sinkpads;
  guint32               pads_cookie;
} GstElement;
|#

(define-cstruct _GstElement
  ([state_lock _GStaticRecMutex-pointer]
   [state_cond _GCond-pointer]
   [state_cookie _guint32]
   [current_state _int]
   [next_state _int]
   [pending_state _int]
   [last_return _GstStateChangeReturn]
   [bus _GstBus-pointer]
   [clock _GstClock-pointer]
   [base_time _GstClockTimeDiff]
   [numpads _guint16]
   [pads _GList-pointer]
   [numsrcpads _guint16]
   [srcpads _GList-pointer]
   [numsinkpads _guint16]
   [sinkpads _GList-pointer]
   [pads_cookie _guint32]))



;;GST ELEMENT FACTORY

;;typedef struct _GstElementFactory GstElementFactory;
(define-cpointer-type _GstElementFactory-pointer)

#|
typedef struct {
  gchar *longname;
  gchar *klass;
  gchar *description;
  gchar *author;
} GstElementDetails;
|#

(define-cstruct _GstElementDetails
  ([longname _string]
   [klass _string]
   [description _string]
   [author _string]))


;;GST PAD

#|typedef enum {
  GST_ACTIVATE_NONE,
  GST_ACTIVATE_PUSH,
  GST_ACTIVATE_PULL
} GstActivateMode;|#

(define _GstActivateMode
  (_enum '( GST_ACTIVATE_NONE GST_ACTIVATE_PUSH GST_ACTIVATE_PULL)))


#|  typedef enum {
  GST_PAD_LINK_OK               =  0,
  GST_PAD_LINK_WRONG_HIERARCHY  = -1,
  GST_PAD_LINK_WAS_LINKED       = -2,
  GST_PAD_LINK_WRONG_DIRECTION  = -3,
  GST_PAD_LINK_NOFORMAT         = -4,
  GST_PAD_LINK_NOSCHED          = -5,
  GST_PAD_LINK_REFUSED          = -6
  } GstPadLinkReturn;|#

(define _GstPadLinkReturn
  (_enum '(GST_PAD_LINK_OK =  0 GST_PAD_LINK_WRONG_HIERARCHY = -1 GST_PAD_LINK_WAS_LINKED = -2 GST_PAD_LINK_WRONG_DIRECTION = -3 GST_PAD_LINK_NOFORMAT = -4 GST_PAD_LINK_NOSCHED = -5 GST_PAD_LINK_REFUSED = -6)))

#|typedef enum {
  /* custom success starts here */
  GST_FLOW_CUSTOM_SUCCESS_2 = 102,
  GST_FLOW_CUSTOM_SUCCESS_1 = 101,
  GST_FLOW_CUSTOM_SUCCESS = 100,

  /* core predefined */
  GST_FLOW_RESEND	  =  1,
  GST_FLOW_OK		  =  0,
  /* expected failures */
  GST_FLOW_NOT_LINKED     = -1,
  GST_FLOW_WRONG_STATE    = -2,
  /* error cases */
  GST_FLOW_UNEXPECTED     = -3,
  GST_FLOW_NOT_NEGOTIATED = -4,
  GST_FLOW_ERROR	  = -5,
  GST_FLOW_NOT_SUPPORTED  = -6,

  /* custom error starts here */
  GST_FLOW_CUSTOM_ERROR   = -100,
  GST_FLOW_CUSTOM_ERROR_1 = -101,
  GST_FLOW_CUSTOM_ERROR_2 = -102
} GstFlowReturn;|#
  

(define _GstFlowReturn
  (_enum '(GST_FLOW_CUSTOM_SUCCESS_2 = 102 GST_FLOW_CUSTOM_SUCCESS_1 = 101 GST_FLOW_CUSTOM_SUCCESS = 100 GST_FLOW_RESEND =  1 GST_FLOW_OK =  0 GST_FLOW_NOT_LINKED = -1 GST_FLOW_WRONG_STATE = -2 GST_FLOW_UNEXPECTED = -3 GST_FLOW_NOT_NEGOTIATED = -4 GST_FLOW_ERROR = -5 GST_FLOW_NOT_SUPPORTED = -6 GST_FLOW_CUSTOM_ERROR = -100 GST_FLOW_CUSTOM_ERROR_1 = -101 GST_FLOW_CUSTOM_ERROR_2 = -102)))



;void                (*GstPadBlockCallback)              (GstPad *pad, gboolean blocked, gpointer user_data);
(define GstPadBlockCallback (_cprocedure '(_GstPad-pointer _gboolean _gpointer) _void))

;GstCaps *           (*GstPadGetCapsFunction)            (GstPad *pad);
(define GstPadGetCapsFunction (_cprocedure '(_GstPad-pointer) _GstCaps-pointer))

;gboolean            (*GstPadSetCapsFunction)            (GstPad *pad, GstCaps *caps);
(define GstPadSetCapsFunction (_cprocedure '(_GstPad-pointer _GstCaps-pointer) _gboolean))

;gboolean            (*GstPadAcceptCapsFunction)         (GstPad *pad, GstCaps *caps);
(define GstPadAcceptCapsFunction (_cprocedure '(_GstPad-pointer _GstCaps-pointer) _gboolean))

;void                (*GstPadFixateCapsFunction)         (GstPad *pad, GstCaps *caps);
(define GstPadFixateCapsFunction (_cprocedure '(_GstPad-pointer _GstCaps-pointer) _void))

;gboolean            (*GstPadActivateFunction)           (GstPad *pad);
(define GstPadActivateFunction (_cprocedure '(_GstPad-pointer) _gboolean))

;gboolean            (*GstPadActivateModeFunction)       (GstPad *pad, gboolean active);
(define GstPadActivateModeFunction (_cprocedure '(_GstPad-pointer _gboolean) _gboolean))

;GstPadLinkReturn    (*GstPadLinkFunction)               (GstPad *pad, GstPad *peer);
(define GstPadLinkFunction (_cprocedure '(_GstPad-pointer _GstPad-pointer) _GstPadLinkReturn))

;void                (*GstPadUnlinkFunction)             (GstPad *pad);
(define GstPadUnlinkFunction (_cprocedure '(_GstPad-pointer) _void))

;GstFlowReturn       (*GstPadChainFunction)              (GstPad *pad, GstBuffer *buffer);
(define GstPadChainFunction (_cprocedure '(_GstPad-pointer _GstBuffer-pointer) _GstFlowReturn))

;gboolean            (*GstPadCheckGetRangeFunction)      (GstPad *pad);
(define GstPadCheckGetRangeFunction (_cprocedure '(_GstPad-pointer) _gboolean))

;GstFlowReturn       (*GstPadGetRangeFunction)           (GstPad *pad, guint64 offset, guint length, GstBuffer **buffer);
(define GstPadGetRangeFunction (_cprocedure '(_GstPad-pointer _guint64 _guint (_ptr io _GstBuffer-pointer)) _GstFlowReturn))

;gboolean            (*GstPadEventFunction)              (GstPad *pad, GstEvent *event);
(define GstPadEventFunction (_cprocedure '(_GstPad-pointer _GstEvent-pointer) _gboolean))

;const GstQueryType * (*GstPadQueryTypeFunction)         (GstPad *pad);
(define GstPadQueryTypeFunction (_cprocedure '(_GstPad-pointer) (_ptr o _GstQueryType)))

;gboolean            (*GstPadQueryFunction)              (GstPad *pad, GstQuery *query);
(define GstPadQueryFunction (_cprocedure '(_GstPad-pointer _GstQuery-pointer) _gboolean))

;GstFlowReturn       (*GstPadBufferAllocFunction)        (GstPad *pad, guint64 offset, guint size, GstCaps *caps, GstBuffer **buf);
(define GstPadBufferAllocFunction (_cprocedure '(_GstPad-pointer _guint64 _guint _GstCaps-pointer (_ptr io _GstBuffer-pointer)) _GstFlowReturn))

;GstIterator *       (*GstPadIterIntLinkFunction)        (GstPad *pad);
(define GstPadIterIntLinkFunction (_cprocedure '(_GstPad-pointer) _GstIterator-pointer))

#|
typedef struct {
  gpointer			element_private;

  GstPadTemplate		*padtemplate;

  GstPadDirection		 direction;

  /* streaming rec_lock */
  GStaticRecMutex		*stream_rec_lock;
  GstTask			*task;
  GMutex			*preroll_lock;
  GCond				*preroll_cond;

  /* block cond, mutex is from the object */
  GCond				*block_cond;
  GstPadBlockCallback		 block_callback;
  gpointer			 block_data;

  /* the pad capabilities */
  GstCaps			*caps;
  GstPadGetCapsFunction		getcapsfunc;
  GstPadSetCapsFunction		setcapsfunc;
  GstPadAcceptCapsFunction	 acceptcapsfunc;
  GstPadFixateCapsFunction	 fixatecapsfunc;

  GstPadActivateFunction	 activatefunc;
  GstPadActivateModeFunction	 activatepushfunc;
  GstPadActivateModeFunction	 activatepullfunc;

  /* pad link */
  GstPadLinkFunction		 linkfunc;
  GstPadUnlinkFunction		 unlinkfunc;
  GstPad			*peer;

  gpointer			 sched_private;

  /* data transport functions */
  GstPadChainFunction		 chainfunc;
  GstPadCheckGetRangeFunction	 checkgetrangefunc;
  GstPadGetRangeFunction	 getrangefunc;
  GstPadEventFunction		 eventfunc;

  GstActivateMode		 mode;

  /* generic query method */
  GstPadQueryTypeFunction	 querytypefunc;
  GstPadQueryFunction		 queryfunc;

  /* internal links */
  GstPadIntLinkFunction		 intlinkfunc;

  GstPadBufferAllocFunction      bufferallocfunc;

  /* whether to emit signals for have-data. counts number
   * of handlers attached. */
  gint				 do_buffer_signals;
  gint				 do_event_signals;

  /* ABI added */
  /* iterate internal links */
  GstPadIterIntLinkFunction     iterintlinkfunc;

  /* free block_data */
  GDestroyNotify block_destroy_data;
} GstPad;
|#

(define-cstruct _GstPad
  ([element_private _gpointer]
   [padtemplate _GstPadTemplate-pointer]
   [direction _GstPadDirection]
   [stream_rec_lock _GStaticRecMutex-pointer]
   [task _GstTask-pointer]
   [preroll_lock _GMutex-pointer]
   [preroll_cond _GCond-pointer]
   [block_cond _GCond-pointer]
   [block_callback GstPadBlockCallback]
   [block_data _gpointer]
   [caps _GstCaps-pointer]
   [getcapsfunc GstPadGetCapsFunction]
   [setcapsfunc GstPadSetCapsFunction]
   [acceptcapsfunc GstPadAcceptCapsFunction]
   [fixatecapsfunc GstPadFixateCapsFunction]
   [activatefunc GstPadActivateFunction]
   [activatepushfunc GstPadActivateModeFunction]
   [activatepullfunc GstPadActivateModeFunction]
   [linkfunc GstPadLinkFunction]
   [unlinkfunc GstPadUnlinkFunction]
   [peer _GstPad-pointer]
   [sched_private _gpointer]
   [chainfunc GstPadChainFunction]
   [checkgetrangefunc GstPadCheckGetRangeFunction]
   [getrangefunc GstPadGetRangeFunction]
   [eventfunc GstPadEventFunction]
   [mode _GstActivateMode]
   [querytypefunc GstPadQueryTypeFunction]
   [queryfunc GstPadQueryFunction]
  ; [intlinkfunc GstPadIntLinkFunction]  ;;deprecated
   [bufferallocfunc GstPadBufferAllocFunction]
   [do_buffer_signals _gint]
   [do_event_signals _gint]
   [iterintlinkfunc GstPadIterIntLinkFunction]
   [block_destroy_data _GDestroyNotify]))




;GST QUERY

#|typedef enum {
  GST_QUERY_NONE = 0,
  GST_QUERY_POSITION,
  GST_QUERY_DURATION,
  GST_QUERY_LATENCY,
  GST_QUERY_JITTER,     /* not in draft-query, necessary? */
  GST_QUERY_RATE,
  GST_QUERY_SEEKING,
  GST_QUERY_SEGMENT,
  GST_QUERY_CONVERT,
  GST_QUERY_FORMATS,
  GST_QUERY_BUFFERING,
  GST_QUERY_CUSTOM,
  GST_QUERY_URI
} GstQueryType;|#

(define _GstQueryType
  (_enum '(GST_QUERY_NONE = 0 GST_QUERY_POSITION GST_QUERY_DURATION GST_QUERY_LATENCY GST_QUERY_JITTER GST_QUERY_RATE GST_QUERY_SEEKING,
  GST_QUERY_SEGMENT GST_QUERY_CONVERT GST_QUERY_FORMATS GST_QUERY_BUFFERING GST_QUERY_CUSTOM GST_QUERY_URI)))


#|
typedef struct {
  GstMiniObject mini_object;
  GstQueryType type;
  GstStructure *structure;
} GstQuery;
|#

(define-cstruct _GstQuery
  ([mini_object _GstMiniObject]
   [type _GstQueryType]
   [structure _GstStructure-pointer]))