#lang racket

(require "gst_base.rkt"
         "gst-structs-ffi.rkt"
         "GstObject-ffi.rkt"
         "GstClock-ffi.rkt")

(provide (all-defined-out))

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


(define GST_STATE_CHANGE_FAILURE 0)
(define GST_STATE_CHANGE_SUCCESS 1)
(define GST_STATE_CHANGE_ASYNC 2)
(define GST_STATE_CHANGE_NO_PREROLL 3)

#|typedef enum
{
  GST_ELEMENT_LOCKED_STATE      = (GST_OBJECT_FLAG_LAST << 0),
  GST_ELEMENT_IS_SINK           = (GST_OBJECT_FLAG_LAST << 1),
  GST_ELEMENT_UNPARENTING       = (GST_OBJECT_FLAG_LAST << 2),
  /* padding */
  GST_ELEMENT_FLAG_LAST         = (GST_OBJECT_FLAG_LAST << 16)
} GstElementFlags;|#

(define GST_ELEMENT_LOCKED_STATE (arithmetic-shift GST_OBJECT_FLAG_LAST 0))
(define GST_ELEMENT_IS_SINK (arithmetic-shift GST_OBJECT_FLAG_LAST 1))
(define GST_ELEMENT_UNPARENTING (arithmetic-shift GST_OBJECT_FLAG_LAST 2))
(define GST_ELEMENT_FLAG_LAST (arithmetic-shift GST_OBJECT_FLAG_LAST 16))



#|typedef enum /*< flags=0 >*/
{
  GST_STATE_CHANGE_NULL_TO_READY        = (GST_STATE_NULL<<3) | GST_STATE_READY,
  GST_STATE_CHANGE_READY_TO_PAUSED      = (GST_STATE_READY<<3) | GST_STATE_PAUSED,
  GST_STATE_CHANGE_PAUSED_TO_PLAYING    = (GST_STATE_PAUSED<<3) | GST_STATE_PLAYING,
  GST_STATE_CHANGE_PLAYING_TO_PAUSED    = (GST_STATE_PLAYING<<3) | GST_STATE_PAUSED,
  GST_STATE_CHANGE_PAUSED_TO_READY      = (GST_STATE_PAUSED<<3) | GST_STATE_READY,
  GST_STATE_CHANGE_READY_TO_NULL        = (GST_STATE_READY<<3) | GST_STATE_NULL
} GstStateChange;|#

(define GST_STATE_CHANGE_NULL_TO_READY (bitwise-ior (arithmetic-shift GST_STATE_NULL 3) GST_STATE_READY))
(define GST_STATE_CHANGE_READY_TO_PAUSED (bitwise-ior (arithmetic-shift GST_STATE_READY 3) GST_STATE_PAUSED))
(define GST_STATE_CHANGE_PAUSED_TO_PLAYING (bitwise-ior (arithmetic-shift GST_STATE_PAUSED 3) GST_STATE_PLAYING))
(define GST_STATE_CHANGE_PLAYING_TO_PAUSED (bitwise-ior (arithmetic-shift GST_STATE_PLAYING 3) GST_STATE_PAUSED))
(define GST_STATE_CHANGE_PAUSED_TO_READY (bitwise-ior (arithmetic-shift GST_STATE_PAUSED 3) GST_STATE_READY))
(define GST_STATE_CHANGE_READY_TO_NULL (bitwise-ior (arithmetic-shift GST_STATE_READY 3) GST_STATE_NULL))





;#define             GST_STATE                           (elem)
;#define             GST_STATE_GET_NEXT                  (cur,pending)
;#define             GST_STATE_NEXT                      (elem)
;#define             GST_STATE_PENDING                   (elem)
;#define             GST_STATE_RETURN                    (elem)
;#define             GST_STATE_TARGET                    (elem)
;#define             GST_STATE_TRANSITION                (cur, next)
;#define             GST_STATE_TRANSITION_CURRENT        (trans)
;#define             GST_STATE_TRANSITION_NEXT           (trans)
;#define             GST_STATE_GET_LOCK                  (elem)
;#define             GST_STATE_GET_COND                  (elem)
;#define             GST_ELEMENT_NAME                    (elem)
;#define             GST_ELEMENT_PARENT                  (elem)
;#define             GST_ELEMENT_BUS                     (elem)
;#define             GST_ELEMENT_CLOCK                   (elem)
;#define             GST_ELEMENT_PADS                    (elem)
;#define             GST_ELEMENT_START_TIME              (elem)
;#define             GST_ELEMENT_ERROR                   (el, domain, code, text, debug)
;#define             GST_ELEMENT_WARNING                 (el, domain, code, text, debug)
;#define             GST_ELEMENT_INFO                    (el, domain, code, text, debug)
;#define             GST_ELEMENT_IS_LOCKED_STATE         (elem)
;#define             gst_element_set_name                (elem, name)
;#define             gst_element_get_name                (elem)
;#define             gst_element_set_parent              (elem, parent)


;void                gst_element_class_add_pad_template  (GstElementClass *klass, GstPadTemplate *templ);
(define-gstreamer gst_element_class_add_pad_template (_fun _GstElementClass-pointer _GstPadTemplate-pointer -> _void))

;GstPadTemplate*     gst_element_class_get_pad_template  (GstElementClass *element_class, const gchar *name);
(define-gstreamer gst_element_class_get_pad_template (_fun _GstElementClass-pointer _string -> _GstPadTemplate-pointer))

;GList*              gst_element_class_get_pad_template_list (GstElementClass *element_class);
(define-gstreamer gst_element_class_get_pad_template_list (_fun _GstElementClass-pointer -> _GList-pointer))

;void                gst_element_class_install_std_props (GstElementClass *klass, const gchar *first_name, ...);
(define-gstreamer gst_element_class_install_std_props (_fun _GstElementClass-pointer _string (_list i _string) -> _void))

;void                gst_element_class_set_details       (GstElementClass *klass, const GstElementDetails *details);
(define-gstreamer gst_element_class_set_details (_fun _GstElementClass-pointer _GstElementDetails-pointer -> _void))

;void         gst_element_class_set_details_simple (GstElementClass *klass, const gchar *longname, const gchar *classification, const gchar *description, const gchar *author);
(define-gstreamer gst_element_class_set_details_simple (_fun _GstElementClass-pointer _string _string _string _string -> _void))

;;GstElement* GstPad* -> gboolean
(define-gstreamer*
  (_fun _GstElement-pointer _GstPad-pointer -> _gboolean)
  gst_element_add_pad gst_element_remove_pad)

;GstPad*             gst_element_get_compatible_pad      (GstElement *element, GstPad *pad, const GstCaps *caps);
(define-gstreamer gst_element_get_compatible_pad (_fun _GstElement-pointer _GstPad-pointer (_or-null _GstCaps-pointer) -> _GstPad-pointer))

;GstPadTemplate*     gst_element_get_compatible_pad_template (GstElement *element, GstPadTemplate *compattempl);
(define-gstreamer gst_element_get_compatible_pad_template (_fun _GstElement-pointer _GstPadTemplate-pointer -> _GstPadTemplate-pointer))

;;GstElement* gchar* -> GstPad*
(define-gstreamer*
  (_fun _GstElement-pointer _string -> (_or-null _GstPad-pointer))
  gst_element_get_request_pad gst_element_get_static_pad)

;void                gst_element_release_request_pad     (GstElement *element, GstPad *pad);
(define-gstreamer gst_element_release_request_pad (_fun _GstElement-pointer _GstPad-pointer -> _void))

;;GstElement* -> GstIterator*
(define-gstreamer*
  (_fun _GstElement-pointer -> _GstIterator-pointer)
  gst_element_iterate_pads gst_element_iterate_sink_pads gst_element_iterate_src_pads)

;gboolean            gst_element_link                    (GstElement *src, GstElement *dest);
(define-gstreamer gst_element_link (_fun _GstElement-pointer _GstElement-pointer -> _gboolean))

;void                gst_element_unlink                  (GstElement *src, GstElement *dest);
(define-gstreamer gst_element_unlink (_fun _GstElement-pointer _GstElement-pointer -> _void))

;gboolean            gst_element_link_many               (GstElement *element_1, GstElement *element_2, ...);

(define (gst_element_link_many list-of-elements)
  (cond 
    ((empty? list-of-elements) 1)
    ((empty? (rest list-of-elements)) 1)
    ((equal? (gst_element_link (first list-of-elements) (second list-of-elements)) 1)
     (gst_element_link_many (rest list-of-elements)))
      (else 0)))            


;void                gst_element_unlink_many             (GstElement *element_1, GstElement *element_2, ...);
(define (gst_element_unlink_many list-of-elements)
  (cond 
    ((not (or (empty? list-of-elements) (empty? (rest list-of-elements))))
      (begin
        (gst_element_unlink (first list-of-elements) (second list-of-elements))
        (gst_element_unlink_many (rest list-of-elements))))))


;gboolean            gst_element_link_pads               (GstElement *src, const gchar *srcpadname, GstElement *dest, const gchar *destpadname);
(define-gstreamer gst_element_link_pads (_fun _GstElement-pointer _string _GstElement-pointer _string -> _gboolean))

;gboolean            gst_element_link_pads_full          (GstElement *src, const gchar *srcpadname, GstElement *dest, const gchar *destpadname, GstPadLinkCheck flags);
;(define-gstreamer gst_element_link_pads_full (_fun _GstElement-pointer _string _GstElement-pointer _string _int -> _gboolean))   NOT IN LIB

;void                gst_element_unlink_pads             (GstElement *src, const gchar *srcpadname, GstElement *dest, const gchar *destpadname);
(define-gstreamer gst_element_unlink_pads (_fun _GstElement-pointer _string _GstElement-pointer _string -> _void))

;gboolean            gst_element_link_pads_filtered      (GstElement *src, const gchar *srcpadname, GstElement *dest, const gchar *destpadname, GstCaps *filter);
(define-gstreamer gst_element_link_pads_filtered (_fun _GstElement-pointer _string _GstElement-pointer _string _GstCaps-pointer -> _gboolean))

;gboolean            gst_element_link_filtered           (GstElement *src, GstElement *dest, GstCaps *filter);
(define-gstreamer gst_element_link_filtered (_fun _GstElement-pointer _GstElement-pointer _GstCaps-pointer -> _gboolean))

;;GstElement* GstClockTime -> void
(define-gstreamer*
  (_fun _GstElement-pointer _GstClockTime -> _void)
  gst_element_set_base_time gst_element_set_start_time)

;;GstElement* -> GstClockTime
(define-gstreamer*
  (_fun _GstElement-pointer -> _GstClockTime)
  gst_element_get_base_time gst_element_get_start_time)

;void                gst_element_set_bus                 (GstElement *element, GstBus *bus);
(define-gstreamer gst_element_set_bus (_fun _GstElement-pointer _GstBus-pointer -> _void))

;GstBus *            gst_element_get_bus                 (GstElement *element);
(define-gstreamer gst_element_get_bus (_fun _GstElement-pointer -> _GstBus-pointer))

;GstElementFactory*  gst_element_get_factory             (GstElement *element);
(define-gstreamer gst_element_get_factory (_fun _GstElement-pointer -> _GstElementFactory-pointer))

;void                gst_element_set_index               (GstElement *element, GstIndex *index);
(define-gstreamer gst_element_set_index (_fun _GstElement-pointer _GstIndex-pointer -> _void))

;GstIndex*           gst_element_get_index               (GstElement *element);
(define-gstreamer gst_element_get_index (_fun _GstElement-pointer -> (_or-null _GstIndex-pointer)))

;gboolean            gst_element_set_clock               (GstElement *element, GstClock *clock);
(define-gstreamer gst_element_set_clock (_fun _GstElement-pointer _GstClock-pointer -> _gboolean))

;GstClock*           gst_element_provide_clock           (GstElement *element);
(define-gstreamer gst_element_provide_clock (_fun _GstElement-pointer -> (_or-null _GstClock-pointer)))

;GstClock*           gst_element_get_clock               (GstElement *element);
(define-gstreamer gst_element_get_clock (_fun _GstElement-pointer -> _GstClock-pointer))

;GstStateChangeReturn  gst_element_set_state             (GstElement *element, GstState state);
(define-gstreamer gst_element_set_state (_fun _GstElement-pointer _int -> _int))

;GstStateChangeReturn  gst_element_get_state             (GstElement *element, GstState *state, GstState *pending, GstClockTime timeout);
(define-gstreamer gst_element_get_state 
  (_fun _GstElement-pointer 
        (state : (_ptr o _int)) (pending : (_ptr o _int)) _GstClockTime 
        -> _int 
        -> (values state pending)))
  
  ;gboolean            gst_element_set_locked_state        (GstElement *element, gboolean locked_state);
  (define-gstreamer gst_element_set_locked_state (_fun _GstElement-pointer _gboolean -> _gboolean))
  
  ;;GstElement* -> gboolean
  (define-gstreamer*
    (_fun _GstElement-pointer -> _gboolean)
    gst_element_is_indexable gst_element_requires_clock gst_element_provides_clock gst_element_is_locked_state gst_element_sync_state_with_parent)
  
  ;;GstElement* -> void
  (define-gstreamer*
    (_fun _GstElement-pointer -> _void)
    gst_element_create_all_pads gst_element_no_more_pads gst_element_abort_state gst_element_lost_state)
  
  
  ;GstStateChangeReturn  gst_element_continue_state        (GstElement *element, GstStateChangeReturn ret);
  (define-gstreamer gst_element_continue_state (_fun _GstElement-pointer _int -> _int))
  
  ;void                gst_element_lost_state_full         (GstElement *element, gboolean new_base_time);
  (define-gstreamer gst_element_lost_state_full (_fun _GstElement-pointer _gboolean -> _void))
  
  ;const gchar*        gst_element_state_get_name          (GstState state);
  (define-gstreamer gst_element_state_get_name (_fun _int -> _string))
  
  ;const gchar *       gst_element_state_change_return_get_name (GstStateChangeReturn state_ret);
  (define-gstreamer gst_element_state_change_return_get_name (_fun _int -> _string))
  
  ;GstStateChangeReturn  gst_element_change_state          (GstElement *element, GstStateChange transition);
  (define-gstreamer gst_element_change_state (_fun _GstElement-pointer _int -> _int))
  
  ;void                gst_element_found_tags              (GstElement *element, GstTagList *list);
  (define-gstreamer gst_element_found_tags (_fun _GstElement-pointer _GstTagList-pointer -> _void))
  
  ;void                gst_element_found_tags_for_pad      (GstElement *element, GstPad *pad, GstTagList *list);
  (define-gstreamer gst_element_found_tags_for_pad (_fun _GstElement-pointer _GstPad-pointer _GstTagList-pointer -> _void))
  
  ;void gst_element_message_full (GstElement *element, GstMessageType type, GQuark domain, gint code, gchar *text, gchar *debug, const gchar *file, const gchar *function, gint line);
  (define-gstreamer gst_element_message_full (_fun _GstElement-pointer _int _GQuark _gint  _string _string _string _string _gint -> _void))
  
  ;gboolean            gst_element_post_message            (GstElement *element, GstMessage *message);
  (define-gstreamer gst_element_post_message (_fun _GstElement-pointer _GstMessage-pointer -> _gboolean))
  
  
  ;const GstQueryType * gst_element_get_query_types        (GstElement *element); ;; binded in common-wrap
  
  ;gboolean            gst_element_query                   (GstElement *element, GstQuery *query);
  (define-gstreamer gst_element_query (_fun _GstElement-pointer _GstQuery-pointer -> _gboolean))
  
  ;gboolean            gst_element_query_convert           (GstElement *element, GstFormat src_format, gint64 src_val, GstFormat *dest_format, gint64 *dest_val);
  (define-gstreamer gst_element_query_convert (_fun _GstElement-pointer _int _gint64 (dest-format : (_ptr io _int)) (result : (_ptr o _gint64)) -> (succeed? : _gboolean) -> (values succeed? dest-format result)))
  
  ;;GstElement* GstFormat* gint64* -> gboolean
  (define-gstreamer*
    (_fun _GstElement-pointer (format : (_ptr io _int)) (result : (_ptr o _gint64)) -> (succeed? : _gboolean) -> (values succeed? format result))
    gst_element_query_position gst_element_query_duration)
  
  ;gboolean            gst_element_send_event              (GstElement *element, GstEvent *event);
  (define-gstreamer gst_element_send_event (_fun _GstElement-pointer _GstEvent-pointer -> _gboolean))
  
  ;gboolean            gst_element_seek_simple             (GstElement *element, GstFormat format, GstSeekFlags seek_flags, gint64 seek_pos);
  (define-gstreamer gst_element_seek_simple (_fun _GstElement-pointer _int _int _gint64 -> _gboolean))
  
  ;gboolean gst_element_seek (GstElement *element, gdouble rate, GstFormat format, GstSeekFlags flags, GstSeekType cur_type, gint64 cur, GstSeekType stop_type, gint64 stop);
  (define-gstreamer gst_element_seek (_fun _GstElement-pointer _gdouble _int _int _int _gint64 _int _gint64 -> _gboolean))
  
  