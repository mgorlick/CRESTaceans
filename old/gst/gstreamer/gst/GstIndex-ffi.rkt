#lang racket

(require ffi/unsafe)
(require "gst_base.rkt"
         "gst-structs-ffi.rkt"
         "GstObject-ffi.rkt")

(provide (all-defined-out))

#|typedef enum {
  GST_INDEX_UNKNOWN,
  GST_INDEX_CERTAIN,
  GST_INDEX_FUZZY
} GstIndexCertainty;|#

(define GST_INDEX_UNKNOWN 0)
(define GST_INDEX_CERTAIN 1)
(define GST_INDEX_FUZZY 2)


#|typedef enum {
  GST_INDEX_ENTRY_ID,
  GST_INDEX_ENTRY_ASSOCIATION,
  GST_INDEX_ENTRY_OBJECT,
  GST_INDEX_ENTRY_FORMAT
} GstIndexEntryType;|#

(define GST_INDEX_ENTRY_ID 0)
(define GST_INDEX_ENTRY_ASSOCIATION 1)
(define GST_INDEX_ENTRY_OBJECT 2)
(define GST_INDEX_ENTRY_FORMAT 3)


#|typedef enum {
  GST_INDEX_LOOKUP_EXACT,
  GST_INDEX_LOOKUP_BEFORE,
  GST_INDEX_LOOKUP_AFTER
} GstIndexLookupMethod;|#

(define GST_INDEX_LOOKUP_EXACT 0)
(define GST_INDEX_LOOKUP_BEFORE 1)
(define GST_INDEX_LOOKUP_AFTER 2)


#|#define             GST_INDEX_NASSOCS                   (entry)
#define             GST_INDEX_ASSOC_FLAGS               (entry)
#define             GST_INDEX_ASSOC_FORMAT              (entry, i)
#define             GST_INDEX_ASSOC_VALUE               (entry, i)
#define             GST_INDEX_FORMAT_FORMAT             (entry)
#define             GST_INDEX_FORMAT_KEY                (entry)
#define             GST_INDEX_ID_INVALID
#define             GST_INDEX_ID_DESCRIPTION            (entry)
#define             GST_INDEX_IS_READABLE               (obj)
#define             GST_INDEX_IS_WRITABLE               (obj)
|#



#|typedef enum {
  GST_ASSOCIATION_FLAG_NONE       = 0,
  GST_ASSOCIATION_FLAG_KEY_UNIT   = (1 << 0),
  GST_ASSOCIATION_FLAG_DELTA_UNIT = (1 << 1),

  /* new flags should start here */
  GST_ASSOCIATION_FLAG_LAST     = (1 << 8)
} GstAssocFlags;|#

(define GST_ASSOCIATION_FLAG_NONE 0)
(define GST_ASSOCIATION_FLAG_KEY_UNIT (arithmetic-shift 1 0))
(define GST_ASSOCIATION_FLAG_DELTA_UNIT (arithmetic-shift 1 1))
(define GST_ASSOCIATION_FLAG_LAST (arithmetic-shift 1 8))


;gboolean            (*GstIndexFilter)                   (GstIndex *index, GstIndexEntry *entry, gpointer user_data);
(define GstIndexFilter (_cprocedure (list _GstIndex-pointer _GstIndexEntry-pointer _gpointer) _gboolean))

#|typedef enum {
  GST_INDEX_RESOLVER_CUSTOM,
  GST_INDEX_RESOLVER_GTYPE,
  GST_INDEX_RESOLVER_PATH
} GstIndexResolverMethod;|#

(define GST_INDEX_RESOLVER_CUSTOM 0)
(define GST_INDEX_RESOLVER_GTYPE 1)
(define GST_INDEX_RESOLVER_PATH 2)


;gboolean            (*GstIndexResolver)                 (GstIndex *index, GstObject *writer, gchar **writer_string, gpointer user_data);
(define GstIndexResolver (_cprocedure (list _GstIndex-pointer _GstObject-pointer (_ptr io _string) _gpointer) _gboolean))


#|typedef enum {
  GST_INDEX_WRITABLE    = (GST_OBJECT_FLAG_LAST << 0),
  GST_INDEX_READABLE    = (GST_OBJECT_FLAG_LAST << 1),

  GST_INDEX_FLAG_LAST   = (GST_OBJECT_FLAG_LAST << 8)
} GstIndexFlags;|#

(define GST_INDEX_WRITABLE (arithmetic-shift GST_OBJECT_FLAG_LAST 0))
(define GST_INDEX_READABLE (arithmetic-shift GST_OBJECT_FLAG_LAST 1))
(define GST_INDEX_FLAG_LAST (arithmetic-shift GST_OBJECT_FLAG_LAST 8))


;GstIndex*           gst_index_new                       (void);
(define-gstreamer gst_index_new (_fun -> _GstIndex-pointer))

;void                gst_index_commit                    (GstIndex *index, gint id);
(define-gstreamer gst_index_commit (_fun _GstIndex-pointer _gint -> _void))

;;GstIndex* -> gint
(define-gstreamer*
  (_fun _GstIndex-pointer -> _gint)
  gst_index_get_group gst_index_new_group)

;gboolean            gst_index_set_group                 (GstIndex *index,gint groupnum);
(define-gstreamer gst_index_set_group (_fun _GstIndex-pointer _gint -> _gboolean))

;void                gst_index_set_certainty             (GstIndex *index, GstIndexCertainty certainty);
(define-gstreamer gst_index_set_certainty (_fun _GstIndex-pointer _int -> _void))

;GstIndexCertainty   gst_index_get_certainty             (GstIndex *index);
(define-gstreamer gst_index_get_certainty (_fun _GstIndex-pointer -> _int))

;void                gst_index_set_filter                (GstIndex *index, GstIndexFilter filter, gpointer user_data);
(define-gstreamer gst_index_set_filter (_fun _GstIndex-pointer GstIndexFilter _gpointer -> _void))

;void                gst_index_set_filter_full           (GstIndex *index, GstIndexFilter filter, gpointer user_data, GDestroyNotify user_data_destroy);
(define-gstreamer gst_index_set_filter_full (_fun _GstIndex-pointer GstIndexFilter _gpointer GDestroyNotify -> _void))

;void                gst_index_set_resolver              (GstIndex *index, GstIndexResolver resolver, gpointer user_data);
(define-gstreamer gst_index_set_resolver (_fun _GstIndex-pointer GstIndexResolver _gpointer -> _void))

;void                gst_index_set_resolver_full         (GstIndex *index, GstIndexResolver resolver, gpointer user_data, GDestroyNotify user_data_destroy);
(define-gstreamer gst_index_set_resolver_full (_fun _GstIndex-pointer GstIndexResolver _gpointer GDestroyNotify -> _void))

;gboolean            gst_index_get_writer_id             (GstIndex *index, GstObject *writer, gint *id);
(define-gstreamer gst_index_get_writer_id (_fun _GstIndex-pointer _GstObject-pointer _gint -> _gboolean))

;GstIndexEntry*      gst_index_add_format                (GstIndex *index, gint id, GstFormat format);
(define-gstreamer gst_index_add_format (_fun _GstIndex-pointer _gint _int -> _GstIndexEntry-pointer))

;GstIndexEntry*      gst_index_add_association           (GstIndex *index, gint id, GstAssocFlags flags, GstFormat format, gint64 value, ...);
(define-gstreamer gst_index_add_association (_fun _GstIndex-pointer _gint _int _int _gint64 (_list i _gint64) -> _GstIndexEntry-pointer))

;GstIndexEntry*      gst_index_add_associationv          (GstIndex *index, gint id, GstAssocFlags flags, gint n, const GstIndexAssociation *list);
(define-gstreamer gst_index_add_associationv (_fun _GstIndex-pointer _gint _int _gint _GstIndexAssociation-pointer -> _GstIndexEntry-pointer))

;GstIndexEntry*      gst_index_add_object                (GstIndex *index, gint id, gchar *key, GType type, gpointer object);
(define-gstreamer gst_index_add_object (_fun _GstIndex-pointer _gint _string _GType _gpointer -> _GstIndexEntry-pointer))

;GstIndexEntry*      gst_index_add_id                    (GstIndex *index, gint id, gchar *description);
(define-gstreamer gst_index_add_id (_fun _GstIndex-pointer _gint _string -> _GstIndexEntry-pointer))

;GstIndexEntry*      gst_index_get_assoc_entry           (GstIndex *index, gint id, GstIndexLookupMethod method, GstAssocFlags flags, GstFormat format, gint64 value);
(define-gstreamer gst_index_get_assoc_entry (_fun _GstIndex-pointer _gint _int _int _int _gint64 -> _GstIndexEntry-pointer))

;GstIndexEntry*      gst_index_get_assoc_entry_full      (GstIndex *index, gint id, GstIndexLookupMethod method, GstAssocFlags flags, GstFormat format, gint64 value, GCompareDataFunc func, gpointer user_data);
(define-gstreamer  gst_index_get_assoc_entry_full (_fun _GstIndex-pointer _gint _int _int _int _gint64 GCompareDataFunc _gpointer -> _GstIndexEntry-pointer))

;GstIndexEntry *     gst_index_entry_copy                (GstIndexEntry *entry);
(define-gstreamer gst_index_entry_copy (_fun _GstIndexEntry-pointer -> _GstIndexEntry-pointer))

;void                gst_index_entry_free                (GstIndexEntry *entry);
(define-gstreamer gst_index_entry_free (_fun _GstIndexEntry-pointer -> _void))

;gboolean            gst_index_entry_assoc_map           (GstIndexEntry *entry, GstFormat format, gint64 *value);
(define-gstreamer gst_index_entry_assoc_map (_fun _GstIndexEntry-pointer _int _gint64 -> _gboolean))
