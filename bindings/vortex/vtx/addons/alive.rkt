#lang racket

(require ffi/unsafe
         "../libvortex.rkt")
(provide (all-defined-out))

(define _VortexAliveFailure
  (_fun _VortexConnection-pointer _long _int -> _void))

(defvtxa* (_fun _VortexConnection-pointer _long _int
                _VortexAliveFailure -> _axl-bool)
  vortex-alive-enable-check)

(defvtxa* (_fun _VortexCtx-pointer -> _axl-bool)
  vortex-alive-init)