#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _VortexCtx-pointer _int -> _void)
  vortex-thread-pool-add
  vortex-thread-pool-init
  vortex-thread-pool-remove)

(defvtx* (_fun _VortexCtx-pointer (_ptr i _int) -> _void)
  vortex-thread-pool-event-stats)

(defvtx* (_fun -> _int)
  vortex-thread-pool-get-num)

(defvtx* (_fun _int -> _void)
  vortex-thread-pool-set-num)

(defvtx* (_fun _VortexCtx-pointer -> _int)
  vortex-thread-pool-get-running-threads)

(defvtx* (_fun _VortexCtx-pointer _long _VortexThreadAsyncEvent _axlPointer 
               _axlPointer -> _int)
  vortex-thread-pool-new-event)

(defvtx* (_fun _VortexCtx-pointer _VortexThreadFunc _axlPointer -> _void)
  vortex-thread-pool-new-task)

(defvtx* (_fun _VortexCtx-pointer _axl-bool -> _void)
  vortex-thread-pool-set-exclusive-pool)

(defvtx* (_fun _VortexCtx-pointer 
               (running : (_ptr o _int))
               (waiting : (_ptr o _int))
               (pending : (_ptr o _int))
               -> _void
               -> (values running waiting pending))
  vortex-thread-pool-stats)