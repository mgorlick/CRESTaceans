#lang racket

(require ffi/unsafe)
(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))



#|#define             gst_buffer_list_is_writable         (list)
#define             gst_buffer_list_make_writable       (list)|#

;GstBuffer *         (*GstBufferListDoFunction)          (GstBuffer *buffer, gpointer user_data); ;;OJO
(define GstBufferListDoFunction (_cprocedure (list _GstBuffer-pointer _gpointer) _GstBuffer-pointer))

;GstBufferList *     gst_buffer_list_new                 (void);
(define-gstreamer gst_buffer_list_new (_fun -> _GstBufferList-pointer))


;void                gst_buffer_list_unref               (GstBufferList *list);
;(define-gstreamer gst_buffer_list_unref (_fun _GstBufferList-pointer -> _void)) NOT IN LIB

;guint               gst_buffer_list_n_groups            (GstBufferList *list);
(define-gstreamer gst_buffer_list_n_groups (_fun _GstBufferList-pointer -> _guint))

#|typedef enum {
  GST_BUFFER_LIST_CONTINUE,
  GST_BUFFER_LIST_SKIP_GROUP,
  GST_BUFFER_LIST_END
} GstBufferListItem;|#

(define GST_BUFFER_LIST_CONTINUE 0)
(define GST_BUFFER_LIST_SKIP_GROUP 1)
(define GST_BUFFER_LIST_END 2)


;GstBufferListItem   (*GstBufferListFunc)                (GstBuffer **buffer, guint group, guint idx, gpointer user_data);
(define GstBufferListFunc (_cprocedure (list (_ptr io _GstBuffer-pointer) _guint _guint _gpointer) _int))

;void                gst_buffer_list_foreach             (GstBufferList *list, GstBufferListFunc func, gpointer user_data);
(define-gstreamer gst_buffer_list_foreach (_fun _GstBufferList-pointer GstBufferListFunc _gpointer -> _void))

;GstBuffer *         gst_buffer_list_get                 (GstBufferList *list, guint group, guint idx);
(define-gstreamer gst_buffer_list_get (_fun _GstBufferList-pointer _guint _guint -> _GstBuffer-pointer))

;GstBufferListIterator * gst_buffer_list_iterate         (GstBufferList *list);
(define-gstreamer gst_buffer_list_iterate (_fun _GstBufferList-pointer -> _GstBufferListIterator-pointer))

;guint               gst_buffer_list_iterator_n_buffers  (const GstBufferListIterator *it);
(define-gstreamer gst_buffer_list_iterator_n_buffers (_fun _GstBufferListIterator-pointer -> _guint))

;void                gst_buffer_list_iterator_add        (GstBufferListIterator *it, GstBuffer *buffer);
(define-gstreamer gst_buffer_list_iterator_add (_fun _GstBufferListIterator-pointer _GstBuffer-pointer -> _void))


;;GstBufferList* -> void
(define-gstreamer*
  (_fun _GstBufferListIterator-pointer -> _void)
  gst_buffer_list_iterator_free gst_buffer_list_iterator_add_group gst_buffer_list_iterator_remove)


;gboolean            gst_buffer_list_iterator_next_group (GstBufferListIterator *it);
(define-gstreamer gst_buffer_list_iterator_next_group (_fun _GstBufferListIterator-pointer -> _gboolean))

;GstBuffer *         gst_buffer_list_iterator_steal      (GstBufferListIterator *it);
(define-gstreamer gst_buffer_list_iterator_steal (_fun _GstBufferListIterator-pointer -> _GstBuffer-pointer))

;void                gst_buffer_list_iterator_take       (GstBufferListIterator *it,GstBuffer *buffer);
(define-gstreamer gst_buffer_list_iterator_take (_fun _GstBufferListIterator-pointer _GstBuffer-pointer -> _void))

;GstBuffer *         gst_buffer_list_iterator_do         (GstBufferListIterator *it, GstBufferListDoFunction do_func, gpointer user_data);
(define-gstreamer gst_buffer_list_iterator_do (_fun _GstBufferListIterator-pointer GstBufferListDoFunction _gpointer -> _GstBuffer-pointer))

;;GstBufferListIterator* -> GstBuffer*
(define-gstreamer*
  (_fun _GstBufferListIterator-pointer -> _GstBuffer-pointer)
  gst_buffer_list_iterator_next gst_buffer_list_iterator_merge_group)
