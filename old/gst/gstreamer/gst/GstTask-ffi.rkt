#lang racket

(require ffi/unsafe)
(require "gst_base.rkt"
         "gst-structs-ffi.rkt")

(provide (all-defined-out))


#|typedef enum {
  GST_TASK_STARTED,
  GST_TASK_STOPPED,
  GST_TASK_PAUSED
} GstTaskState;|#

(define GST_TASK_STARTED 0)
(define GST_TASK_STOPPED 1)
(define GST_TASK_PAUSED 2)


;void                (*GstTaskFunction)                  (void *data);
(define GstTaskFunction (_cprocedure empty _void))


#|#define             GST_TASK_BROADCAST                  (task)
#define             GST_TASK_GET_COND                   (task)
#define             GST_TASK_GET_LOCK                   (task)
#define             GST_TASK_SIGNAL                     (task)
#define             GST_TASK_STATE                      (task)
#define             GST_TASK_WAIT                       (task)|#

  

;GstTask*            gst_task_create                     (GstTaskFunction func, gpointer data);
(define-gstreamer gst_task_create (_fun GstTaskFunction _gpointer -> _GstTask-pointer))

;void                gst_task_set_lock                   (GstTask *task, GStaticRecMutex *mutex);
(define-gstreamer gst_task_set_lock (_fun _GstTask-pointer _GStaticRecMutex-pointer -> _void))

;void                gst_task_set_priority               (GstTask *task, GThreadPriority priority);
(define-gstreamer gst_task_set_priority (_fun _GstTask-pointer _int -> _void))

;void                gst_task_set_pool                   (GstTask *task, GstTaskPool *pool);
(define-gstreamer gst_task_set_pool (_fun _GstTask-pointer _GstTaskPool-pointer -> _void))

;GstTaskPool *       gst_task_get_pool                   (GstTask *task);
(define-gstreamer gst_task_get_pool (_fun _GstTask-pointer -> _GstTaskPool-pointer)) 
  
;void                gst_task_set_thread_callbacks       (GstTask *task, GstTaskThreadCallbacks *callbacks, gpointer user_data, GDestroyNotify notify);
(define-gstreamer gst_task_set_thread_callbacks (_fun _GstTask-pointer _GstTaskThreadCallbacks-pointer _gpointer GDestroyNotify -> _void)) 

;GstTaskState        gst_task_get_state                  (GstTask *task);
(define-gstreamer gst_task_get_state (_fun _GstTask-pointer -> _int)) 

;gboolean            gst_task_set_state                  (GstTask *task, GstTaskState state);
(define-gstreamer gst_task_set_state (_fun _GstTask-pointer _int -> _gboolean)) 

;;GstTask* -> gboolean
(define-gstreamer*
  (_fun _GstTask-pointer -> _gboolean)
  gst_task_pause gst_task_start gst_task_stop gst_task_join)

;void                gst_task_cleanup_all                (void);
(define-gstreamer gst_task_cleanup_all (_fun -> _void)) 



