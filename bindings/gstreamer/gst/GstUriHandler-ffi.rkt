#lang racket

(require "gst_base.rkt"
         "GstStructs-ffi.rkt")

(provide (all-defined-out))

;;typedef struct _GstURIHandler GstURIHandler;
(define-cpointer-type _GstURIHandler-pointer)

#|
typedef struct {
  GTypeInterface parent;
  /* querying capabilities */
  GstURIType		(* get_type)		(void);
  gchar **		(* get_protocols) (void);
  /* using the interface */
  G_CONST_RETURN gchar *(* get_uri)		(GstURIHandler * handler); ;;OJO!!!
  gboolean		(* set_uri)		(GstURIHandler * handler, const gchar *	 uri);

  GstURIType		(* get_type_full) (GType type);
  gchar **		(* get_protocols_full) (GType type);
} GstURIHandlerInterface;
|#

(define-cstruct _GstURIHandlerInterface
  ([parent _GTypeInterface-pointer]
   [get_type (_ptr io (_fun -> _GstURIType))]
   [get_protocols (_ptr io (_fun -> (_ptr io _string)))]
   [get_uri (_ptr io (_fun _GstURIHandler-pointer -> _string))]
   [set_uri (_ptr io (_fun _GstURIHandler-pointer _string -> _gboolean))]
   [get_type_full (_ptr io (_fun (_ptr io _GType) -> _GstURIType))]
   [get_protocols_full (_ptr io (_fun (_ptr io _GType) -> (_ptr io _string)))]))


#|typedef enum {
  GST_URI_UNKNOWN,
  GST_URI_SINK,
  GST_URI_SRC
} GstURIType;|#

(define _GstURIType
  (_enum '(GST_URI_UNKNOWN GST_URI_SINK GST_URI_SRC)))


#|#define             GST_URI_TYPE_IS_VALID               (type)|#

;gboolean            gst_uri_protocol_is_valid           (const gchar *protocol);
(define-gstreamer gst_uri_protocol_is_valid (_fun _string -> _gboolean))

;gboolean            gst_uri_protocol_is_supported       (const GstURIType type, const gchar *protocol);
(define-gstreamer gst_uri_protocol_is_supported (_fun _GstURIType _string -> _gboolean))

;gboolean            gst_uri_is_valid                    (const gchar *uri);
(define-gstreamer gst_uri_is_valid (_fun _string -> _gboolean))

;gboolean            gst_uri_has_protocol                (const gchar *uri, const gchar *protocol);
(define-gstreamer gst_uri_has_protocol (_fun _string _string -> _gboolean))

;;gchar* -> gchar*
(define-gstreamer*
  (_fun _string -> _string)
  gst_uri_get_protocol gst_uri_get_location)

;gchar *             gst_uri_construct                   (const gchar *protocol, const gchar *location);
(define-gstreamer gst_uri_construct (_fun _string _string -> _string))

;GstElement *        gst_element_make_from_uri           (const GstURIType type, const gchar *uri, const gchar *elementname);
(define-gstreamer gst_element_make_from_uri (_fun _GstURIType _string _string -> _GstElement-pointer))

;guint               gst_uri_handler_get_uri_type        (GstURIHandler *handler);
(define-gstreamer gst_uri_handler_get_uri_type (_fun _GstURIHandler-pointer -> _guint))

;gchar **            gst_uri_handler_get_protocols       (GstURIHandler *handler);
(define-gstreamer gst_uri_handler_get_protocols (_fun _GstURIHandler-pointer -> (_ptr io _string)))

;gchar *             gst_uri_handler_get_uri             (GstURIHandler *handler);
(define-gstreamer gst_uri_handler_get_uri (_fun _GstURIHandler-pointer -> _string))

;gboolean            gst_uri_handler_set_uri             (GstURIHandler *handler, const gchar *uri);
(define-gstreamer gst_uri_handler_set_uri (_fun _GstURIHandler-pointer _string -> _gboolean))

;void                gst_uri_handler_new_uri             (GstURIHandler *handler, const gchar *uri);
(define-gstreamer gst_uri_handler_new_uri (_fun _GstURIHandler-pointer _string -> _void))
