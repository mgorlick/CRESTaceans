#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _VortexCtx-pointer -> _void)
  vortex-io-init)

(defvtx* (_fun _VortexCtx-pointer -> _VortexIoWaitingType)
  vortex-io-waiting-get-current)

(defvtx* (_fun _VortexCtx-pointer _axlPointer _VortexIoDispatchFunc _int _axlPointer -> _void)
  vortex-io-waiting-invoke-dispatch)

(defvtx* (_fun _VortexCtx-pointer _axlPointer -> _axl-bool)
  vortex-io-waiting-invoke-have-dispatch)

(defvtx* (_fun _VortexIoWaitingType -> _axl-bool)
  vortex-io-waiting-is-available)

(defvtx* (_fun _VortexCtx-pointer _VortexIoAddToFdGroup -> _void)
  vortex-io-waiting-set-add-to-fd-group)

(defvtx* (_fun _VortexCtx-pointer _VortexIoClearFdGroup -> _void)
  vortex-io-waiting-set-clear-fd-group)

(defvtx* (_fun _VortexCtx-pointer _VortexIoCreateFdGroup -> _void)
  vortex-io-waiting-set-create-fd-group)

(defvtx* (_fun _VortexCtx-pointer _VortexIoDestroyFdGroup -> _void)
  vortex-io-waiting-set-destroy-fd-group)

(defvtx* (_fun _VortexCtx-pointer _VortexIoDispatch -> _void)
  vortex-io-waiting-set-dispatch)

(defvtx* (_fun _VortexCtx-pointer _VortexIoHaveDispatch -> _void)
  vortex-io-waiting-set-have-dispatch)

(defvtx* (_fun _VortexCtx-pointer _VortexIoIsSetFdGroup -> _void)
  vortex-io-waiting-set-is-set-fd-group)

(defvtx* (_fun _VortexCtx-pointer _VortexIoWaitOnFdGroup -> _void)
  vortex-io-waiting-set-wait-on-fd-group)

(defvtx* (_fun _VortexCtx-pointer _VortexIoWaitingType -> _axl-bool)
  vortex-io-waiting-use)