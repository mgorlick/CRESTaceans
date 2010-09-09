#lang racket

(require "gst_base.rkt"
         "GstObject-ffi.rkt")

(provide (all-defined-out))

;;typedef struct _GstTaskPool GstTaskPool;
(define-cpointer-type _GstTaskPool-pointer)

#|
typedef struct {
  GstObjectClass parent_class;
  void      (*prepare)  (GstTaskPool *pool, GError **error);
  void      (*cleanup)  (GstTaskPool *pool);
  gpointer  (*push)     (GstTaskPool *pool, GstTaskPoolFunction func, gpointer user_data, GError **error);
  void      (*join)     (GstTaskPool *pool, gpointer id);
} GstTaskPoolClass;
|#

(define-cstruct _GstTaskPoolClass
  ([parent_class _GstObjectClass]
   [prepare (_ptr io (_fun _GstTaskPool-pointer (_ptr io _GError-pointer) -> _void))]
   [cleanup (_ptr io (_fun _GstTaskPool-pointer -> _void))]
   [push (_ptr io (_fun _GstTaskPool-pointer GstTaskPoolFunction _gpointer (_ptr io _GError-pointer) -> _gpointer))]
   [join (_ptr io (_fun _GstTaskPool-pointer _gpointer -> _void))]))


;void                (*GstTaskPoolFunction)              (void *data);
(define GstTaskPoolFunction (_cprocedure '() _void))

;GstTaskPool *       gst_task_pool_new                   (void);
(define-gstreamer gst_task_pool_new (_fun -> _GstTaskPool-pointer))

;void                gst_task_pool_prepare               (GstTaskPool *pool, GError **error);
(define-gstreamer gst_task_pool_prepare (_fun _GstTaskPool-pointer (_ptr io _GError-pointer) -> _void))

;gpointer            gst_task_pool_push                  (GstTaskPool *pool, GstTaskPoolFunction func, gpointer user_data, GError **error);
(define-gstreamer gst_task_pool_push (_fun _GstTaskPool-pointer GstTaskPoolFunction _gpointer (_ptr io _GError-pointer) -> _gpointer))

;void                gst_task_pool_join                  (GstTaskPool *pool, gpointer id);
(define-gstreamer gst_task_pool_join (_fun _GstTaskPool-pointer _gpointer -> _void))

;void                gst_task_pool_cleanup               (GstTaskPool *pool);
(define-gstreamer gst_task_pool_cleanup (_fun _GstTaskPool-pointer -> _void))
