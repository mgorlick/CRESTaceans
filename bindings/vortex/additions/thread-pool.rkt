#lang racket

(require (only-in ffi/unsafe cpointer?)
         "../vtx/module.rkt")
(provide (all-defined-out))

;; thread pool replacement for vortex library

; original vortex library uses a thread pool of pthreads with a queue to perform callbacks
; in-order, plus processing events. we don't really care about thread allocation overhead
; from within racket, so we can just spawn threads to deal with a callback when a request
; to call back is submitted.

; thread_pool_new_task: VortexCtx-pointer, VortexThreadFunc, pointer -> int
(define/contract (rkt:vortex-thread-pool-new-task ctx fun data)
  (VortexCtx*? procedure? cpointer? . -> . integer?)
  (thread (lambda () (fun data)))
  axl-true)

; in a similar fashion to new_task, just spawn a thread for the new event handler.
; the long is microseconds, so convert to ms, wait and pop the event after the 
; correct amount of time. if the handler returns axl_false, do it again after the 
; same amount of ms; if not, just end

; thread_pool_new_event: VortexCtx-pointer, long, VortexThreadAsyncEvent, pointer, pointer -> int
(define/contract (rkt:vortex-thread-pool-new-event context delay-time handler-fun data1 data2)
  (VortexCtx*? number? procedure? cpointer? cpointer? . -> . integer?)
  (thread
   (lambda ()
     (let loop ()
       (sleep (/ delay-time 1000))
       (let ([stop-going (handler-fun context data1 data2)])
         (if (vtx-false? stop-going)
             (void)
             (loop))))))
  0)