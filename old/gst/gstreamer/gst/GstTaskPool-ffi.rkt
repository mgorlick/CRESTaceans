#lang racket

(require ffi/unsafe)
(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))



;void                (*GstTaskPoolFunction)              (void *data);
(define GstTaskPoolFunction (_cprocedure (list) _void))

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
