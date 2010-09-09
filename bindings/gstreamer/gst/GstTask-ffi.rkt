#lang racket

(require "gst_base.rkt"
         "GstTaskPool-ffi.rkt")

(provide (all-defined-out))


#|typedef enum {
  GST_TASK_STARTED,
  GST_TASK_STOPPED,
  GST_TASK_PAUSED
} GstTaskState;|#

(define _GstTaskState
  (_enum '(GST_TASK_STARTED GST_TASK_STOPPED GST_TASK_PAUSED)))


;void                (*GstTaskFunction)                  (void *data);
(define GstTaskFunction (_cprocedure '() _void))

#|
typedef struct {
  GstTaskState     state;
  GCond           *cond;
  GStaticRecMutex *lock;
  GstTaskFunction  func;
  gpointer         data;
  gboolean         running;
} GstTask;
|#

(define-cstruct _GstTask
  ([state _GstTaskState]
   [cond _GCond-pointer]
   [lock _GStaticRecMutex-pointer]
   [func GstTaskFunction]
   [data _gpointer]
   [running _gboolean]))



#|#define             GST_TASK_BROADCAST                  (task)
#define             GST_TASK_GET_COND                   (task)
#define             GST_TASK_GET_LOCK                   (task)
#define             GST_TASK_SIGNAL                     (task)
#define             GST_TASK_STATE                      (task)
#define             GST_TASK_WAIT                       (task)|#

#|typedef struct {
  /* manage the lifetime of the thread */
  void      (*enter_thread)     (GstTask *task, GThread *thread, gpointer user_data);
  void      (*leave_thread)     (GstTask *task, GThread *thread, gpointer user_data);
} GstTaskThreadCallbacks;|#

(define-cstruct _GstTaskThreadCallbacks
  ([enter_thread (_ptr io (_fun _GstTask-pointer _GThread-pointer _gpointer -> _void))]
   [leave_thread (_ptr io (_fun _GstTask-pointer _GThread-pointer _gpointer -> _void))]))
  

;GstTask*            gst_task_create                     (GstTaskFunction func, gpointer data);
(define-gstreamer gst_task_create (_fun GstTaskFunction _gpointer -> _GstTask-pointer))

;void                gst_task_set_lock                   (GstTask *task, GStaticRecMutex *mutex);
(define-gstreamer gst_task_set_lock (_fun _GstTask-pointer _GStaticRecMutex-pointer -> _void))

;void                gst_task_set_priority               (GstTask *task, GThreadPriority priority);
(define-gstreamer gst_task_set_priority (_fun _GstTask-pointer _GThreadPriority -> _void))

;void                gst_task_set_pool                   (GstTask *task, GstTaskPool *pool);
(define-gstreamer gst_task_set_pool (_fun _GstTask-pointer _GstTaskPool-pointer -> _void))

;GstTaskPool *       gst_task_get_pool                   (GstTask *task);
(define-gstreamer gst_task_get_pool (_fun _GstTask-pointer -> _GstTaskPool-pointer)) 
  
;void                gst_task_set_thread_callbacks       (GstTask *task, GstTaskThreadCallbacks *callbacks, gpointer user_data, GDestroyNotify notify);
(define-gstreamer gst_task_set_thread_callbacks (_fun _GstTask-pointer _GstTaskThreadCallbacks-pointer _gpointer _GDestroyNotify -> _void)) 

;GstTaskState        gst_task_get_state                  (GstTask *task);
(define-gstreamer gst_task_get_state (_fun _GstTask-pointer -> _GstTaskState)) 

;gboolean            gst_task_set_state                  (GstTask *task, GstTaskState state);
(define-gstreamer gst_task_set_state (_fun _GstTask-pointer _GstTaskState -> _gboolean)) 

;;GstTask* -> gboolean
(define-gstreamer*
  (_fun _GstTask-pointer -> _gboolean)
  gst_task_pause gst_task_start gst_task_stop gst_task_join)

;void                gst_task_cleanup_all                (void);
(define-gstreamer gst_task_cleanup_all (_fun -> _void)) 



