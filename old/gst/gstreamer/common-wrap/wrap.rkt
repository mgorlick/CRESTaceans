#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         "../gst/gstreamer.rkt")

(provide (except-out (all-defined-out) df))

(define-ffi-definer df (ffi-lib "libracket-gst" "1.0"))

(df signal_connect_pad_added (_fun _GstElement-pointer _GstElement-pointer -> _void))

(define VoidMessageType (_cprocedure (list _GstMessage-pointer) _void))

(df signal_connect_message (_fun _gpointer _string VoidMessageType _gpointer -> _void))

(df gst_message_type (_fun _GstMessage-pointer -> _GstMessageType))

(df gst_message_unref_w (_fun _GstMessage-pointer -> _void))

(df print_gst_time_format (_fun _gint64 _gint64 -> _void))

(df pointer_is_null (_fun _pointer -> _int))


;;;;getters and setters

(df get_GstBin_numchildren (_fun _GstBin-pointer -> _int))

(df get_GstElement_name (_fun _GstElement-pointer -> _string))

(df get_GstPad_name (_fun _GstPad-pointer -> _string))

(df set_Gst_Buffer_Size (_fun _GstBuffer-pointer _int -> _void))

(df get_Gst_Buffer_Size (_fun _GstBuffer-pointer -> _int))

(df set_Gst_Buffer_Data (_fun _GstBuffer-pointer (_ptr i _guint8) -> _void))

(df get_Gst_Buffer_Data (_fun _GstBuffer-pointer -> (_ptr o _guint8)))

(df set_Gst_Buffer_Mallocdata (_fun _GstBuffer-pointer (_ptr i _guint8) -> _void))

(df get_Gst_Buffer_Mallocdata (_fun _GstBuffer-pointer -> (_ptr o _guint8)))

(df set_Gst_Buffer_Offset (_fun _GstBuffer-pointer _guint -> _void))

(df get_Gst_Buffer_Offset (_fun _GstBuffer-pointer -> _guint))

(df set_Gst_Buffer_Offset_End (_fun _GstBuffer-pointer _guint -> _void))

(df get_Gst_Buffer_Offset_End (_fun _GstBuffer-pointer -> _guint))

(df get_Gst_Buffer_Timestamp (_fun _GstBuffer-pointer -> _GstClockTime))

(df set_Gst_Buffer_Timestamp (_fun _GstBuffer-pointer _GstClockTime -> _void))

(df set_Gst_Buffer_Caps (_fun _GstBuffer-pointer _GstCaps-pointer -> _void))

(df get_Gst_Buffer_Caps (_fun _GstBuffer-pointer -> _GstCaps-pointer))

(df get_Gst_Pad_Template_Name_Template (_fun _GstPadTemplate-pointer -> _string))

(df get_gst_element_get_name (_fun _GstElement-pointer -> _string))

(df set_gst_element_set_name (_fun _GstElement-pointer _string -> _void))

(df get_Gst_State (_fun _GstElement-pointer -> _int))

(df get_Gst_State_Next (_fun _GstElement-pointer -> _int))

(df get_Gst_State_Return (_fun _GstElement-pointer -> _int))

(df get_gst_pad_get_name (_fun _GstPad-pointer -> _string))

(df get_Gst_Version_Major (_fun -> _int))

(df get_Gst_Version_Minor (_fun -> _int))

(df get_Gst_Version_Micro (_fun -> _int))

(df get_Gst_Version_Nano (_fun -> _int))

(df get_GST_CORE_ERROR (_fun -> _guint32))

(df get_GST_LIBRARY_ERROR (_fun -> _guint32))

(df get_GST_RESOURCE_ERROR (_fun -> _guint32))

(df get_GST_STREAM_ERROR (_fun -> _guint32))

(df get_GST_ERROR_SYSTEM (_fun -> _guint32))

(df get_gst_caps_refcount (_fun _GstCaps-pointer -> _uint))

(df get_gst_make_fourcc (_fun _gchar _gchar _gchar _gchar -> _guint32))

(df get_gst_type_fourcc (_fun -> _GType))

(df get_gst_static_caps (_fun _string -> _GstStaticCaps-pointer))