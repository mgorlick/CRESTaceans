#lang racket

(require "gst_base.rkt"
         "GstMiniObject-ffi.rkt"
         "GstClock-ffi.rkt"
         "GstCaps-ffi.rkt")

(provide (all-defined-out))

;;typedef struct {
;;  GstMiniObject          mini_object;
;;  /* pointer to data and its size */
;;  guint8                *data;
;;  guint                  size;
;;  /* timestamp */
;;  GstClockTime           timestamp;
;;  GstClockTime           duration;
;;  /* the media type of this buffer */
;;  GstCaps               *caps;
;;  /* media specific offset */
;;  guint64                offset;
;;  guint64                offset_end;
;;  guint8                *malloc_data;
;;  /* ABI Added */
;;  GFreeFunc              free_func;
;;  GstBuffer             *parent;
;;} GstBuffer;

(define-cstruct _GstBuffer
  ([mini_object _GstMiniObject]
   [data (_ptr io _guint8)]
   [size _guint]
   [timestamp _GstClockTime]
   [duration _GstClockTime]
   [caps _GstCaps-pointer]
   [offset _guint64]
   [offset_end _guint64]
   [malloc_data (_ptr io _guint8)]
   [free_func _GFreeFunc]
   [parent _GstBuffer-pointer]))


#|typedef enum {
  GST_BUFFER_FLAG_READONLY   = GST_MINI_OBJECT_FLAG_READONLY,
  GST_BUFFER_FLAG_PREROLL    = (GST_MINI_OBJECT_FLAG_LAST << 0),
  GST_BUFFER_FLAG_DISCONT    = (GST_MINI_OBJECT_FLAG_LAST << 1),
  GST_BUFFER_FLAG_IN_CAPS    = (GST_MINI_OBJECT_FLAG_LAST << 2),
  GST_BUFFER_FLAG_GAP        = (GST_MINI_OBJECT_FLAG_LAST << 3),
  GST_BUFFER_FLAG_DELTA_UNIT = (GST_MINI_OBJECT_FLAG_LAST << 4),
  GST_BUFFER_FLAG_MEDIA1     = (GST_MINI_OBJECT_FLAG_LAST << 5),
  GST_BUFFER_FLAG_MEDIA2     = (GST_MINI_OBJECT_FLAG_LAST << 6),
  GST_BUFFER_FLAG_MEDIA3     = (GST_MINI_OBJECT_FLAG_LAST << 7),
  GST_BUFFER_FLAG_LAST       = (GST_MINI_OBJECT_FLAG_LAST << 8)
} GstBufferFlag;|#

(define GST_BUFFER_FLAG_READONLY GST_MINI_OBJECT_FLAG_READONLY)
(define GST_BUFFER_FLAG_PREROLL (arithmetic-shift GST_MINI_OBJECT_FLAG_LAST 0))
(define GST_BUFFER_FLAG_DISCONT (arithmetic-shift GST_MINI_OBJECT_FLAG_LAST 1))
(define GST_BUFFER_FLAG_IN_CAPS (arithmetic-shift GST_MINI_OBJECT_FLAG_LAST 2))
(define GST_BUFFER_FLAG_GAP (arithmetic-shift GST_MINI_OBJECT_FLAG_LAST 3))
(define GST_BUFFER_FLAG_DELTA_UNIT (arithmetic-shift GST_MINI_OBJECT_FLAG_LAST 4))
(define GST_BUFFER_FLAG_MEDIA1 (arithmetic-shift GST_MINI_OBJECT_FLAG_LAST 5))
(define GST_BUFFER_FLAG_MEDIA2 (arithmetic-shift GST_MINI_OBJECT_FLAG_LAST 6))
(define GST_BUFFER_FLAG_MEDIA3 (arithmetic-shift GST_MINI_OBJECT_FLAG_LAST 7))
(define GST_BUFFER_FLAG_LAST (arithmetic-shift GST_MINI_OBJECT_FLAG_LAST 8))


#|typedef enum {
  GST_BUFFER_COPY_FLAGS      = (1 << 0),
  GST_BUFFER_COPY_TIMESTAMPS = (1 << 1),
  GST_BUFFER_COPY_CAPS       = (1 << 2)
} GstBufferCopyFlags;|#

(define GST_BUFFER_COPY_FLAGS (arithmetic-shift 1 0))
(define GST_BUFFER_COPY_TIMESTAMPS (arithmetic-shift 1 1))
(define GST_BUFFER_COPY_CAPS (arithmetic-shift 1 2))


#|#define             GST_BUFFER_FLAGS                    (buf)
#define             GST_BUFFER_FLAG_IS_SET              (buf, flag)
#define             GST_BUFFER_FLAG_SET                 (buf, flag)
#define             GST_BUFFER_FLAG_UNSET               (buf, flag)
#define             GST_BUFFER_DATA                     (buf)
#define             GST_BUFFER_MALLOCDATA               (buf)
#define             GST_BUFFER_FREE_FUNC                (buf)
#define             GST_BUFFER_SIZE                     (buf)
#define             GST_BUFFER_TIMESTAMP                (buf)
#define             GST_BUFFER_DURATION                 (buf)
#define             GST_BUFFER_CAPS                     (buf)
#define             GST_BUFFER_OFFSET                   (buf)
#define             GST_BUFFER_OFFSET_END               (buf)
#define             GST_BUFFER_OFFSET_NONE
#define             GST_BUFFER_DURATION_IS_VALID        (buffer)
#define             GST_BUFFER_TIMESTAMP_IS_VALID       (buffer)
#define             GST_BUFFER_OFFSET_IS_VALID          (buffer)
#define             GST_BUFFER_OFFSET_END_IS_VALID      (buffer)
#define             GST_BUFFER_IS_DISCONT               (buffer)
#define             GST_BUFFER_TRACE_NAME
#define             gst_buffer_set_data                 (buf, data, size)
#define             GST_BUFFER_COPY_ALL
#define             gst_buffer_is_writable              (buf)
#define             gst_buffer_make_writable            (buf)
#define             gst_buffer_replace                  (obuf, nbuf)|#


;GstBuffer *         gst_buffer_new                      (void);
(define-gstreamer gst_buffer_new (_fun -> _GstBuffer-pointer))

;;guint -> GstBuffer*
(define-gstreamer*
  (_fun _guint -> _GstBuffer-pointer)
  gst_buffer_new_and_alloc gst_buffer_try_new_and_alloc)

;;GstBuffer* -> GstBuffer*
(define-gstreamer*
  (_fun _GstBuffer-pointer -> _GstBuffer-pointer)
  gst_buffer_make_metadata_writable) ;;gst_buffer_ref and gst_buffer_copy NOT IN LIB


;void                gst_buffer_unref                    (GstBuffer *buf);  NOT IN LIB
;(define-gstreamer gst_buffer_unref (_fun _GstBuffer-pointer -> _void))

;void                gst_buffer_copy_metadata            (GstBuffer *dest, const GstBuffer *src, GstBufferCopyFlags flags);
(define-gstreamer gst_buffer_copy_metadata (_fun _GstBuffer-pointer _GstBuffer-pointer _int -> _void))

;gboolean            gst_buffer_is_metadata_writable     (GstBuffer *buf);
(define-gstreamer gst_buffer_is_metadata_writable (_fun _GstBuffer-pointer -> _gboolean))

;GstCaps*            gst_buffer_get_caps                 (GstBuffer *buffer);
(define-gstreamer gst_buffer_get_caps (_fun _GstBuffer-pointer -> _GstCaps-pointer))

;void                gst_buffer_set_caps                 (GstBuffer *buffer, GstCaps *caps);
(define-gstreamer gst_buffer_set_caps (_fun _GstBuffer-pointer _GstCaps-pointer -> _void))

;GstBuffer*          gst_buffer_create_sub               (GstBuffer *parent, guint offset, guint size);
(define-gstreamer gst_buffer_create_sub (_fun _GstBuffer-pointer _guint _guint -> _GstBuffer-pointer))

;gboolean            gst_buffer_is_span_fast             (GstBuffer *buf1, GstBuffer *buf2);
(define-gstreamer gst_buffer_is_span_fast (_fun _GstBuffer-pointer _GstBuffer-pointer -> _gboolean))

;GstBuffer*          gst_buffer_span                     (GstBuffer *buf1,  guint32 offset, GstBuffer *buf2, guint32 len);
(define-gstreamer gst_buffer_span (_fun _GstBuffer-pointer _guint32 _GstBuffer-pointer _guint32 -> _GstBuffer-pointer))

;void                gst_buffer_stamp                    (GstBuffer *dest, const GstBuffer *src);
(define-gstreamer gst_buffer_stamp (_fun _GstBuffer-pointer _GstBuffer-pointer -> _void))

;;GstBuffer* GstBuffer* -> GstBuffer*
(define-gstreamer*
  (_fun _GstBuffer-pointer _GstBuffer-pointer -> _GstBuffer-pointer)
  gst_buffer_join gst_buffer_merge)


