#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _VortexCtx-pointer _int -> _void)
  vortex-thread-pool-add
  vortex-thread-pool-init
  vortex-thread-pool-remove)

(defvtx* (_fun _VortexCtx-pointer (_ptr io _int) -> _void)
  vortex-thread-pool-event-stats)

(defvtx* (_fun -> _void)
  vortex-thread-pool-get-num)

(defvtx* (_fun _int -> _void)
  vortex-thread-pool-set-num)

(defvtx* (_fun _VortexCtx-pointer _long _VortexThreadAsyncEvent _axlPointer _axlPointer -> _int)
  vortex-thread-pool-new-event)

(defvtx* (_fun _VortexCtx-pointer _VortexThreadFunc _axlPointer -> _void)
  vortex-thread-pool-new-task)

(defvtx* (_fun _VortexCtx-pointer _axl-bool -> _void)
  vortex-thread-pool-set-exclusive-pool)

(defvtx* (_fun _VortexCtx-pointer (_ptr io _int) (_ptr io _int) (_ptr io _int) -> _void)
  vortex-thread-pool-stats)