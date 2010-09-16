#lang racket

(require "gst_base.rkt"
         "GstStructs-ffi.rkt")

(provide (all-defined-out))

;GQuark              gst_parse_error_quark               (void);
(define-gstreamer gst_parse_error_quark (_fun -> _GQuark))

#|#define             GST_PARSE_ERROR|#


#|typedef enum
{
  GST_PARSE_ERROR_SYNTAX,
  GST_PARSE_ERROR_NO_SUCH_ELEMENT,
  GST_PARSE_ERROR_NO_SUCH_PROPERTY,
  GST_PARSE_ERROR_LINK,
  GST_PARSE_ERROR_COULD_NOT_SET_PROPERTY,
  GST_PARSE_ERROR_EMPTY_BIN,
  GST_PARSE_ERROR_EMPTY
} GstParseError;|#

(define _GstParseError
  (_enum '( GST_PARSE_ERROR_SYNTAX GST_PARSE_ERROR_NO_SUCH_ELEMENT GST_PARSE_ERROR_NO_SUCH_PROPERTY GST_PARSE_ERROR_LINK GST_PARSE_ERROR_COULD_NOT_SET_PROPERTY GST_PARSE_ERROR_EMPTY_BIN GST_PARSE_ERROR_EMPTY)))  

;typedef struct _GstParseContext GstParseContext;
(define-cpointer-type _GstParseContext-pointer)


#|typedef enum
{
  GST_PARSE_FLAG_NONE = 0,
  GST_PARSE_FLAG_FATAL_ERRORS = (1 << 0)
} GstParseFlags;|#

(define GST_PARSE_FLAG_NONE  0)
(define GST_PARSE_FLAG_FATAL_ERRORS (arithmetic-shift 1 0))


; GstElement *        gst_parse_launch                   
; (const gchar *pipeline_description, GError **error);
(define-gstreamer gst_parse_launch (_fun _string
                                         (error : (_or-null (_ptr o _GError-pointer)) = #f) 
                                         -> (pipeline : _GstElement-pointer)
                                         -> (values pipeline error)))

; GstElement *        gst_parse_launch_full               
; (const gchar *pipeline_description, GstParseContext *context, GstParseFlags flags, GError **error);
(define-gstreamer gst_parse_launch_full (_fun _string _GstParseContext-pointer _int 
                                              (error : (_ptr o _GError-pointer))
                                              -> (pipeline : _GstElement-pointer)
                                              -> (values pipeline error)))

; GstElement *        gst_parse_launchv                   
; (const gchar **argv, GError **error);
(define-gstreamer gst_parse_launchv (_fun (_ptr io _string) ; XXX
                                          (error : (_ptr o _GError-pointer)) 
                                          -> (pipeline : _GstElement-pointer)
                                          -> (values pipeline error)))

;GstElement *        gst_parse_launchv_full              
; (const gchar **argv, GstParseContext *context, GstParseFlags flags, GError **error);
(define-gstreamer gst_parse_launchv_full (_fun (_ptr io _string) ; XXX
                                               _GstParseContext-pointer _int
                                               (error : (_ptr o _GError-pointer))
                                               -> (pipeline : _GstElement-pointer)
                                               -> (values pipeline error)))

;GstElement *        gst_parse_bin_from_description      
; (const gchar *bin_description, gboolean ghost_unlinked_pads, GError **err);
(define-gstreamer gst_parse_bin_from_description (_fun _string _gboolean 
                                                       (error : (_ptr o _GError-pointer)) 
                                                       -> (pipeline : _GstElement-pointer)
                                                       -> (values pipeline error)))

;GstElement * gst_parse_bin_from_description_full 
; (const gchar *bin_description, gboolean ghost_unlinked_pads, GstParseContext *context, GstParseFlags flags, GError **err);
(define-gstreamer gst_parse_bin_from_description_full (_fun _string _gboolean _GstParseContext-pointer _int 
                                                            (error : (_ptr o _GError-pointer))
                                                            -> (pipeline : _GstElement-pointer)
                                                            -> (values pipeline error)))

;GstParseContext *   gst_parse_context_new               (void);
(define-gstreamer gst_parse_context_new (_fun -> _GstParseContext-pointer))

;void                gst_parse_context_free              (GstParseContext *context);
(define-gstreamer gst_parse_context_free (_fun _GstParseContext-pointer -> _void))

;gchar **            gst_parse_context_get_missing_elements (GstParseContext *context);
(define-gstreamer gst_parse_context_get_missing_elements (_fun _GstParseContext-pointer -> (_ptr io _string)))
