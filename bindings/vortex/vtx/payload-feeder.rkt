#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _string _axl-bool -> _VortexPayloadFeeder-pointer)
  vortex-payload-feeder-file)

(defvtx* (_fun _VortexPayloadFeeder-pointer _VortexCtx-pointer -> _axl-bool)
  vortex-payload-feeder-is-finished)

(defvtx* (_fun _VortexPayloadFeeder-pointer _VortexCtx-pointer -> _void)
  vortex-payload-feeder-free)

(defvtx* (_fun _VortexPayloadFeeder-pointer _VortexCtx-pointer _int _string -> _int)
  vortex-payload-feeder-get-content)

(defvtx* (_fun _VortexPayloadFeeder-pointer _VortexCtx-pointer -> _int)
  vortex-payload-feeder-get-pending-size)