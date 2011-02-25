#lang racket

(require ffi/unsafe)
(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))


#|typedef enum {
  GST_ITERATOR_ITEM_SKIP = 0,
  GST_ITERATOR_ITEM_PASS = 1,
  GST_ITERATOR_ITEM_END		= 2
} GstIteratorItem;|#

(define GST_ITERATOR_ITEM_SKIP 0)
(define GST_ITERATOR_ITEM_PASS 1) 
(define GST_ITERATOR_ITEM_END 2)


#|typedef enum {
  GST_ITERATOR_DONE = 0,
  GST_ITERATOR_OK = 1,
  GST_ITERATOR_RESYNC = 2,
  GST_ITERATOR_ERROR = 3
} GstIteratorResult;|#

(define GST_ITERATOR_DONE 0)
(define GST_ITERATOR_OK 1)
(define GST_ITERATOR_RESYNC 2)
(define GST_ITERATOR_ERROR 3)


;void                (*GstIteratorDisposeFunction)       (gpointer owner);
(define GstIteratorDisposeFunction (_cprocedure (list _gpointer) _void))


;GstIteratorResult   (*GstIteratorNextFunction)          (GstIterator *it, gpointer *result);
(define GstIteratorNextFunction (_cprocedure (list _GstIterator-pointer (_ptr io _gpointer)) _int))

;GstIteratorItem     (*GstIteratorItemFunction)          (GstIterator *it, gpointer item);
(define GstIteratorItemFunction (_cprocedure (list _GstIterator-pointer _gpointer) _int))

;void                (*GstIteratorResyncFunction)        (GstIterator *it);
(define GstIteratorResyncFunction (_cprocedure (list _GstIterator-pointer) _void))

;void                (*GstIteratorFreeFunction)          (GstIterator *it);
(define GstIteratorFreeFunction (_cprocedure (list _GstIterator-pointer) _void))

;gboolean            (*GstIteratorFoldFunction)          (gpointer item, GValue *ret, gpointer user_data);
(define GstIteratorFoldFunction (_cprocedure (list _gpointer _GValue-pointer _gpointer) _gboolean))

;gpointer            (*GstCopyFunction)                  (gpointer object);
(define GstCopyFunction (_cprocedure (list _gpointer) _gpointer))


#|#define             GST_ITERATOR                        (it)
#define             GST_ITERATOR_LOCK                   (it)
#define             GST_ITERATOR_COOKIE                 (it)
#define             GST_ITERATOR_ORIG_COOKIE            (it)|#


;GstIterator* gst_iterator_new (guint size, GType type, GMutex *lock, guint32 *master_cookie, GstIteratorNextFunction next, GstIteratorItemFunction item, GstIteratorResyncFunction resync, GstIteratorFreeFunction free);
(define-gstreamer gst_iterator_new (_fun _guint _GType _GMutex-pointer _guint32 GstIteratorNextFunction GstIteratorItemFunction GstIteratorResyncFunction GstIteratorFreeFunction -> _GstIterator-pointer))

;GstIterator* gst_iterator_new_list (GType type, GMutex *lock, guint32 *master_cookie, GList **list, gpointer owner, GstIteratorItemFunction item, GstIteratorDisposeFunction free);
(define-gstreamer gst_iterator_new_list (_fun _GType _GMutex-pointer _guint32 (_ptr io _GList-pointer) _gpointer GstIteratorItemFunction GstIteratorDisposeFunction -> _GstIterator-pointer))

;GstIterator* gst_iterator_new_single (GType type, gpointer object, GstCopyFunction copy, GFreeFunc free);
(define-gstreamer gst_iterator_new_single (_fun _GType _gpointer GstCopyFunction GFreeFunc -> _GstIterator-pointer))

;GstIteratorResult   gst_iterator_next                   (GstIterator *it, gpointer *elem);
(define-gstreamer gst_iterator_next (_fun _GstIterator-pointer (_ptr io _gpointer) -> _int))

;void                gst_iterator_push                   (GstIterator *it, GstIterator *other);
(define-gstreamer gst_iterator_push (_fun _GstIterator-pointer _GstIterator-pointer -> _void))

;GstIterator*        gst_iterator_filter                 (GstIterator *it, GCompareFunc func, gpointer user_data);
(define-gstreamer gst_iterator_filter (_fun _GstIterator-pointer GCompareFunc _gpointer -> _GstIterator-pointer))

;GstIteratorResult   gst_iterator_fold                   (GstIterator *it, GstIteratorFoldFunction func, GValue *ret, gpointer user_data);
(define-gstreamer gst_iterator_fold (_fun _GstIterator-pointer GstIteratorFoldFunction _GValue-pointer _gpointer -> _int))

;GstIteratorResult   gst_iterator_foreach                (GstIterator *it, GFunc func, gpointer user_data);
(define-gstreamer gst_iterator_foreach (_fun _GstIterator-pointer GFunc _gpointer -> _int))

;gpointer            gst_iterator_find_custom            (GstIterator *it, GCompareFunc func, gpointer user_data);
(define-gstreamer gst_iterator_find_custom (_fun _GstIterator-pointer GCompareFunc _gpointer -> _gpointer))
