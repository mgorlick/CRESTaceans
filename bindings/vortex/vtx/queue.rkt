#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _VortexQueue-pointer -> _void)
  vortex-queue-free)

(defvtx* (_fun _VortexQueue-pointer -> _uint)
  vortex-queue-get-length)

(defvtx* (_fun _VortexQueue-pointer _axlPointer -> _axl-bool)
  vortex-queue-head-push
  vortex-queue-push)

(defvtx* (_fun _VortexQueue-pointer -> _axl-bool)
  vortex-queue-is-empty)

(defvtx* (_fun -> _VortexQueue-pointer)
  vortex-queue-new)

(defvtx* (_fun _VortexQueue-pointer -> _axlPointer)
  vortex-queue-peek
  vortex-queue-pop)