#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _VortexAsyncQueue-pointer _VortexAsyncQueueForeach _axlPointer -> _void)
  vortex-async-queue-foreach)

(defvtx* (_fun _VortexAsyncQueue-pointer -> _int)
  vortex-async-queue-items
  vortex-async-queue-length
  vortex-async-queue-waiters)

(defvtx* (_fun _VortexAsyncQueue-pointer -> _void)
  vortex-async-queue-lock
  vortex-async-queue-ref
  vortex-async-queue-unlock
  vortex-async-queue-unref)

(defvtx* (_fun -> _VortexAsyncQueue-pointer)
  vortex-async-queue-new)

(defvtx* (_fun _VortexAsyncQueue-pointer -> _axlPointer)
  vortex-async-queue-pop)

(defvtx* (_fun _VortexAsyncQueue-pointer _axlPointer -> _void)
  vortex-async-queue-priority-push
  vortex-async-queue-push
  vortex-async-queue-unlocked-push)

(defvtx* (_fun (_ptr io _VortexAsyncQueue-pointer) -> _void)
  vortex-async-queue-safe-unref)

(defvtx* (_fun _VortexAsyncQueue-pointer _long -> _axlPointer)
  vortex-async-queue-timedpop)

(defvtx* (_fun _VortexCond-pointer -> _void)
  vortex-cond-broadcast
  vortex-cond-destroy
  vortex-cond-signal)

(defvtx* (_fun _VortexCond-pointer -> _axl-bool)
  vortex-cond-create)

(defvtx* (_fun _VortexCond-pointer _VortexMutex-pointer _long -> _axl-bool)
  vortex-cond-timedwait)

(defvtx* (_fun _VortexCond-pointer _VortexMutex-pointer -> _axl-bool)
  vortex-cond-wait)

(defvtx* (_fun _VortexMutex-pointer -> _axl-bool)
  vortex-mutex-create
  vortex-mutex-destroy)

(defvtx* (_fun _VortexMutex-pointer -> _void)
  vortex-mutex-lock
  vortex-mutex-unlock)

(defvtx* (_fun _VortexThread-pointer _VortexThreadFunc _axlPointer -> _axl-bool) ; XXX ...
  vortex-thread-create)

(defvtx* (_fun _VortexThread-pointer _axl-bool -> _axl-bool)
  vortex-thread-destroy)

(defvtx* (_fun _VortexThreadCreateFunc -> _void)
  vortex-thread-set-create)

(defvtx* (_fun _VortexThreadDestroyFunc -> _void)
  vortex-thread-set-destroy)