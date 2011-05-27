#lang racket

(require ffi/unsafe)
(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))


#|typedef enum {
  GST_URI_UNKNOWN,
  GST_URI_SINK,
  GST_URI_SRC
} GstURIType;|#

(define GST_URI_UNKNOWN 0)
(define GST_URI_SINK 1)
(define GST_URI_SRC 2)


#|#define             GST_URI_TYPE_IS_VALID               (type)|#

;gboolean            gst_uri_protocol_is_valid           (const gchar *protocol);
(define-gstreamer gst_uri_protocol_is_valid (_fun _string -> _gboolean))

;gboolean            gst_uri_protocol_is_supported       (const GstURIType type, const gchar *protocol);
(define-gstreamer gst_uri_protocol_is_supported (_fun _int _string -> _gboolean))

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
(define-gstreamer gst_element_make_from_uri (_fun _int _string _string -> _GstElement-pointer))

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
