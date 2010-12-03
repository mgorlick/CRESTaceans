#lang racket

(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))


#|typedef enum {
  GST_TAG_MERGE_UNDEFINED,
  GST_TAG_MERGE_REPLACE_ALL,
  GST_TAG_MERGE_REPLACE,
  GST_TAG_MERGE_APPEND,
  GST_TAG_MERGE_PREPEND,
  GST_TAG_MERGE_KEEP,
  GST_TAG_MERGE_KEEP_ALL,
  /* add more */
  GST_TAG_MERGE_COUNT
} GstTagMergeMode;|#

(define GST_TAG_MERGE_UNDEFINED 0)
(define GST_TAG_MERGE_REPLACE_ALL 1)
(define GST_TAG_MERGE_REPLACE 2)
(define GST_TAG_MERGE_APPEND 3)
(define GST_TAG_MERGE_PREPEND 4)
(define GST_TAG_MERGE_KEEP 5)
(define GST_TAG_MERGE_KEEP_ALL 6)
(define GST_TAG_MERGE_COUNT 7)


#|typedef enum {
  GST_TAG_FLAG_UNDEFINED,
  GST_TAG_FLAG_META,
  GST_TAG_FLAG_ENCODED,
  GST_TAG_FLAG_DECODED,
  GST_TAG_FLAG_COUNT
} GstTagFlag;|#

(define GST_TAG_FLAG_UNDEFINED 0)
(define GST_TAG_FLAG_META 1)
(define GST_TAG_FLAG_ENCODED 2)
(define GST_TAG_FLAG_DECODED 3)
(define GST_TAG_FLAG_COUNT 4)



;void                (*GstTagForeachFunc)                (const GstTagList *list, const gchar *tag, gpointer user_data);
(define GstTagForeachFunc (_cprocedure (list _GstTagList-pointer _string _gpointer) _void))

;void                (*GstTagMergeFunc)                  (GValue *dest, const GValue *src);
(define GstTagMergeFunc (_cprocedure (list _GValue-pointer _GValue-pointer) _void))

#|#define             GST_TAG_TITLE
#define             GST_TAG_TITLE_SORTNAME
#define             GST_TAG_ARTIST
#define             GST_TAG_ARTIST_SORTNAME
#define             GST_TAG_ALBUM
#define             GST_TAG_ALBUM_SORTNAME
#define             GST_TAG_ALBUM_ARTIST
#define             GST_TAG_ALBUM_ARTIST_SORTNAME
#define             GST_TAG_DATE
#define             GST_TAG_GENRE
#define             GST_TAG_COMMENT
#define             GST_TAG_EXTENDED_COMMENT
#define             GST_TAG_TRACK_NUMBER
#define             GST_TAG_TRACK_COUNT
#define             GST_TAG_ALBUM_VOLUME_NUMBER
#define             GST_TAG_ALBUM_VOLUME_COUNT
#define             GST_TAG_LOCATION
#define             GST_TAG_HOMEPAGE
#define             GST_TAG_DESCRIPTION
#define             GST_TAG_VERSION
#define             GST_TAG_ISRC
#define             GST_TAG_ORGANIZATION
#define             GST_TAG_COPYRIGHT
#define             GST_TAG_COPYRIGHT_URI
#define             GST_TAG_COMPOSER
#define             GST_TAG_CONTACT
#define             GST_TAG_LICENSE
#define             GST_TAG_LICENSE_URI
#define             GST_TAG_PERFORMER
#define             GST_TAG_DURATION
#define             GST_TAG_CODEC
#define             GST_TAG_VIDEO_CODEC
#define             GST_TAG_AUDIO_CODEC
#define             GST_TAG_SUBTITLE_CODEC
#define             GST_TAG_CONTAINER_FORMAT
#define             GST_TAG_BITRATE
#define             GST_TAG_NOMINAL_BITRATE
#define             GST_TAG_MINIMUM_BITRATE
#define             GST_TAG_MAXIMUM_BITRATE
#define             GST_TAG_SERIAL
#define             GST_TAG_ENCODER
#define             GST_TAG_ENCODER_VERSION
#define             GST_TAG_TRACK_GAIN
#define             GST_TAG_TRACK_PEAK
#define             GST_TAG_ALBUM_GAIN
#define             GST_TAG_ALBUM_PEAK
#define             GST_TAG_REFERENCE_LEVEL
#define             GST_TAG_LANGUAGE_CODE
#define             GST_TAG_IMAGE
#define             GST_TAG_PREVIEW_IMAGE
#define             GST_TAG_ATTACHMENT
#define             GST_TAG_BEATS_PER_MINUTE
#define             GST_TAG_KEYWORDS
#define             GST_TAG_GEO_LOCATION_NAME
#define             GST_TAG_GEO_LOCATION_LATITUDE
#define             GST_TAG_GEO_LOCATION_LONGITUDE
#define             GST_TAG_GEO_LOCATION_ELEVATION
#define             GST_TAG_GEO_LOCATION_CITY
#define             GST_TAG_GEO_LOCATION_COUNTRY
#define             GST_TAG_GEO_LOCATION_SUBLOCATION
#define             GST_TAG_GEO_LOCATION_MOVEMENT_DIRECTION
#define             GST_TAG_GEO_LOCATION_MOVEMENT_SPEED
#define             GST_TAG_GEO_LOCATION_CAPTURE_DIRECTION
#define             GST_TAG_SHOW_NAME
#define             GST_TAG_SHOW_SORTNAME
#define             GST_TAG_SHOW_EPISODE_NUMBER
#define             GST_TAG_SHOW_SEASON_NUMBER
#define             GST_TAG_LYRICS
#define             GST_TAG_COMPOSER_SORTNAME
#define             GST_TAG_GROUPING
#define             GST_TAG_USER_RATING
#define             GST_TAG_DEVICE_MANUFACTURER
#define             GST_TAG_DEVICE_MODEL
#define             GST_TAG_IMAGE_ORIENTATION|#


;void                gst_tag_register                    (const gchar *name, GstTagFlag flag, GType type, const gchar *nick, const gchar *blurb, GstTagMergeFunc func);
(define-gstreamer gst_tag_register (_fun _string _int _GType _string _string GstTagMergeFunc -> _void))

;;GValue* GValue* -> void
(define-gstreamer*
  (_fun _GValue-pointer _GValue-pointer -> _void)
  gst_tag_merge_use_first gst_tag_merge_strings_with_comma)

;;gchar* -> gboolean
(define-gstreamer*
  (_fun _string -> _gboolean)
  gst_tag_exists gst_tag_is_fixed)

;GType               gst_tag_get_type                    (const gchar *tag);
(define-gstreamer gst_tag_get_type (_fun _string -> _GType))

;;gchar* -> gchar*
(define-gstreamer*
  (_fun _string -> _string)
  gst_tag_get_nick gst_tag_get_description)

;GstTagFlag          gst_tag_get_flag                    (const gchar *tag);
(define-gstreamer gst_tag_get_flag (_fun _string -> _int))

;GstTagList *        gst_tag_list_new                    (void);
(define-gstreamer gst_tag_list_new (_fun -> _GstTagList-pointer))

;GstTagList *        gst_tag_list_new_full               (const gchar *tag, ...);
(define-gstreamer gst_tag_list_new_full (_fun _string (_list i _string) -> _GstTagList-pointer))

;GstTagList *        gst_tag_list_new_full_valist        (va_list var_args);
(define-gstreamer gst_tag_list_new_full_valist (_fun (_list i _string) -> _GstTagList-pointer))

;gboolean            gst_is_tag_list                     (gconstpointer p);
(define-gstreamer gst_is_tag_list (_fun _gconstpointer -> _gboolean))

;gboolean            gst_tag_list_is_empty               (const GstTagList *list);
(define-gstreamer gst_tag_list_is_empty (_fun _GstTagList-pointer -> _gboolean))

;GstTagList *        gst_tag_list_copy                   (const GstTagList *list);
(define-gstreamer gst_tag_list_copy (_fun _GstTagList-pointer -> _GstTagList-pointer))

;void                gst_tag_list_insert                 (GstTagList *into, const GstTagList *from, GstTagMergeMode mode);
(define-gstreamer gst_tag_list_insert (_fun _GstTagList-pointer _GstTagList-pointer _int -> _void))

;GstTagList *        gst_tag_list_merge                  (const GstTagList *list1, const GstTagList *list2, GstTagMergeMode mode);
(define-gstreamer gst_tag_list_merge (_fun _GstTagList-pointer _GstTagList-pointer _int -> _GstTagList-pointer))

;void                gst_tag_list_free                   (GstTagList *list);
(define-gstreamer gst_tag_list_free (_fun _GstTagList-pointer -> _void))

;guint               gst_tag_list_get_tag_size           (const GstTagList *list, const gchar *tag);
(define-gstreamer gst_tag_list_get_tag_size (_fun _GstTagList-pointer _string -> _guint))

;;GstTagList* GstTagMergeMode gchar* ... -> void
(define-gstreamer*
  (_fun _GstTagList-pointer _int _string (_list i _string) -> _void)
  gst_tag_list_add gst_tag_list_add_values)

;void                gst_tag_list_add_value              (GstTagList *list, GstTagMergeMode mode, const gchar *tag, const GValue *value);
(define-gstreamer gst_tag_list_add_value (_fun _GstTagList-pointer _int _string _GValue-pointer -> _void))

;;GstTagList* GstTagMergeMode gchar* va_list -> void
(define-gstreamer*
  (_fun _GstTagList-pointer _int _string (_list i _string) -> _void)
  gst_tag_list_add_valist gst_tag_list_add_valist_values)

;void                gst_tag_list_remove_tag             (GstTagList *list, const gchar *tag);
(define-gstreamer gst_tag_list_remove_tag (_fun _GstTagList-pointer _string -> _void))

;void                gst_tag_list_foreach                (const GstTagList *list, GstTagForeachFunc func, gpointer user_data);
(define-gstreamer gst_tag_list_foreach (_fun _GstTagList-pointer GstTagForeachFunc _gpointer -> _void))

;const GValue *      gst_tag_list_get_value_index        (const GstTagList *list, const gchar *tag, guint index);
(define-gstreamer gst_tag_list_get_value_index (_fun _GstTagList-pointer _string _guint -> _GValue-pointer))

;gboolean            gst_tag_list_copy_value             (GValue *dest, const GstTagList *list, const gchar *tag);
(define-gstreamer gst_tag_list_copy_value (_fun _GValue-pointer _GstTagList-pointer _string -> _gboolean))

;gboolean            gst_tag_list_get_char               (const GstTagList *list, const gchar *tag, gchar *value);
(define-gstreamer gst_tag_list_get_char (_fun _GstTagList-pointer _string _string -> _gboolean))

;gboolean            gst_tag_list_get_char_index         (const GstTagList *list, const gchar *tag, guint index, gchar *value);
(define-gstreamer gst_tag_list_get_char_index (_fun _GstTagList-pointer _string _guint _string -> _gboolean))

;gboolean            gst_tag_list_get_uchar              (const GstTagList *list, const gchar *tag, guchar *value);
(define-gstreamer gst_tag_list_get_uchar (_fun _GstTagList-pointer _string (_ptr io _guchar) -> _gboolean))

;gboolean            gst_tag_list_get_uchar_index        (const GstTagList *list, const gchar *tag, guint index, guchar *value);
(define-gstreamer gst_tag_list_get_uchar_index (_fun _GstTagList-pointer _string _guint (_ptr io _guchar) -> _gboolean))

;gboolean            gst_tag_list_get_boolean            (const GstTagList *list, const gchar *tag, gboolean *value);
(define-gstreamer gst_tag_list_get_boolean (_fun _GstTagList-pointer _string (_ptr io _gboolean) -> _gboolean))

;gboolean            gst_tag_list_get_boolean_index      (const GstTagList *list, const gchar *tag, guint index, gboolean *value);
(define-gstreamer gst_tag_list_get_boolean_index (_fun _GstTagList-pointer _string _guint (_ptr io _gboolean) -> _gboolean))

;gboolean            gst_tag_list_get_int                (const GstTagList *list, const gchar *tag, gint *value);
(define-gstreamer gst_tag_list_get_int (_fun _GstTagList-pointer _string (_ptr io _gint) -> _gboolean))

;gboolean            gst_tag_list_get_int_index          (const GstTagList *list, const gchar *tag, guint index, gint *value);
(define-gstreamer gst_tag_list_get_int_index (_fun _GstTagList-pointer _string _guint (_ptr io _gint) -> _gboolean))

;gboolean            gst_tag_list_get_uint               (const GstTagList *list, const gchar *tag, guint *value);
(define-gstreamer gst_tag_list_get_uint (_fun _GstTagList-pointer _string (_ptr io _guint) -> _gboolean))

;gboolean            gst_tag_list_get_uint_index         (const GstTagList *list, const gchar *tag, guint index, guint *value);
(define-gstreamer gst_tag_list_get_uint_index (_fun _GstTagList-pointer _string _guint (_ptr io _guint) -> _gboolean))

;gboolean            gst_tag_list_get_long               (const GstTagList *list, const gchar *tag, glong *value);
(define-gstreamer gst_tag_list_get_long (_fun _GstTagList-pointer _string (_ptr io _glong) -> _gboolean))

;gboolean            gst_tag_list_get_long_index         (const GstTagList *list, const gchar *tag, guint index, glong *value);
(define-gstreamer gst_tag_list_get_long_index (_fun _GstTagList-pointer _string _guint (_ptr io _glong) -> _gboolean))

;gboolean            gst_tag_list_get_ulong              (const GstTagList *list, const gchar *tag, gulong *value);
(define-gstreamer gst_tag_list_get_ulong (_fun _GstTagList-pointer _string (_ptr io _gulong) -> _gboolean))

;gboolean            gst_tag_list_get_ulong_index        (const GstTagList *list, const gchar *tag, guint index, gulong *value);
(define-gstreamer gst_tag_list_get_ulong_index (_fun _GstTagList-pointer _string _guint (_ptr io _gulong) -> _gboolean))

;gboolean            gst_tag_list_get_int64              (const GstTagList *list, const gchar *tag, gint64 *value);
(define-gstreamer gst_tag_list_get_int64 (_fun _GstTagList-pointer _string (_ptr io _gint64) -> _gboolean))

;gboolean            gst_tag_list_get_int64_index        (const GstTagList *list, const gchar *tag, guint index, gint64 *value);
(define-gstreamer gst_tag_list_get_int64_index (_fun _GstTagList-pointer _string _guint (_ptr io _gint64) -> _gboolean))

;gboolean            gst_tag_list_get_uint64             (const GstTagList *list, const gchar *tag, guint64 *value);
(define-gstreamer gst_tag_list_get_uint64 (_fun _GstTagList-pointer _string (_ptr io _guint64) -> _gboolean))

;gboolean            gst_tag_list_get_uint64_index       (const GstTagList *list, const gchar *tag, guint index, guint64 *value);
(define-gstreamer gst_tag_list_get_uint64_index (_fun _GstTagList-pointer _string _guint (_ptr io _guint64) -> _gboolean))

;gboolean            gst_tag_list_get_float              (const GstTagList *list, const gchar *tag, gfloat *value);
(define-gstreamer gst_tag_list_get_float (_fun _GstTagList-pointer _string (_ptr io _gfloat) -> _gboolean))

;gboolean            gst_tag_list_get_float_index        (const GstTagList *list, const gchar *tag, guint index, gfloat *value);
(define-gstreamer gst_tag_list_get_float_index (_fun _GstTagList-pointer _string _guint (_ptr io _gfloat) -> _gboolean))

;gboolean            gst_tag_list_get_double             (const GstTagList *list, const gchar *tag, gdouble *value);
(define-gstreamer gst_tag_list_get_double (_fun _GstTagList-pointer _string (_ptr io _gdouble) -> _gboolean))

;gboolean            gst_tag_list_get_double_index       (const GstTagList *list, const gchar *tag, guint index, gdouble *value);
(define-gstreamer gst_tag_list_get_double_index (_fun _GstTagList-pointer _string _guint (_ptr io _gdouble) -> _gboolean))

;gboolean            gst_tag_list_get_string             (const GstTagList *list, const gchar *tag, gchar **value);
(define-gstreamer gst_tag_list_get_string (_fun _GstTagList-pointer _string (_ptr io _string) -> _gboolean))

;gboolean            gst_tag_list_get_string_index       (const GstTagList *list, const gchar *tag, guint index, gchar **value);
(define-gstreamer gst_tag_list_get_string_index (_fun _GstTagList-pointer _string _guint (_ptr io _string) -> _gboolean))

;gboolean            gst_tag_list_peek_string_index      (const GstTagList *list, const gchar *tag, guint index, const gchar **value);
;(define-gstreamer gst_tag_list_peek_string_index (_fun _GstTagList-pointer _string _guint (_ptr io _string) -> _gboolean)) NOT IN LIB

;gboolean            gst_tag_list_get_pointer            (const GstTagList *list, const gchar *tag, gpointer *value);
(define-gstreamer gst_tag_list_get_pointer (_fun _GstTagList-pointer _string (_ptr io _gpointer) -> _gboolean))

;gboolean            gst_tag_list_get_pointer_index      (const GstTagList *list, const gchar *tag, guint index, gpointer *value);
(define-gstreamer gst_tag_list_get_pointer_index (_fun _GstTagList-pointer _string _guint (_ptr io _gpointer) -> _gboolean))

;gboolean            gst_tag_list_get_date               (const GstTagList *list, const gchar *tag, GDate **value);
(define-gstreamer gst_tag_list_get_date (_fun _GstTagList-pointer _string (_ptr io _GDate-pointer) -> _gboolean))

;gboolean            gst_tag_list_get_date_index         (const GstTagList *list, const gchar *tag, guint index, GDate **value);
(define-gstreamer gst_tag_list_get_date_index (_fun _GstTagList-pointer _string _guint (_ptr io _GDate-pointer) -> _gboolean))

;gboolean            gst_tag_list_get_buffer             (const GstTagList *list, const gchar *tag, GstBuffer **value);
(define-gstreamer gst_tag_list_get_buffer (_fun _GstTagList-pointer _string (_ptr io _GstBuffer-pointer) -> _gboolean))

;gboolean            gst_tag_list_get_buffer_index       (const GstTagList *list, const gchar *tag, guint index, GstBuffer **value);
(define-gstreamer gst_tag_list_get_buffer_index (_fun _GstTagList-pointer _string _guint (_ptr io _GstBuffer-pointer) -> _gboolean))

