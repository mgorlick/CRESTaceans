#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _VortexCtx-pointer -> _axlList-pointer)
  vortex-profiles-get-actual-list
  vortex-profiles-get-actual-list-ref)

(defvtx* (_fun _VortexCtx-pointer _string -> _int)
  vortex-profiles-get-automatic-mime)

(defvtx* (_fun _VortexCtx-pointer _string -> _string)
  vortex-profiles-get-mime-type
  vortex-profiles-get-transfer-encoding)

(defvtx* (_fun _VortexCtx-pointer _string -> _axl-bool)
  vortex-profiles-is-defined-close
  vortex-profiles-is-defined-received
  vortex-profiles-is-defined-start
  vortex-profiles-is-registered)

(defvtx* (_fun _VortexCtx-pointer _string _VortexOnStartChannel _axlPointer
               _VortexOnCloseChannel _axlPointer _VortexOnFrameReceived _axlPointer -> _axl-bool)
  vortex-profiles-register)

(defvtx* (_fun _VortexCtx-pointer _string _VortexOnStartChannelExtended _axlPointer -> _axl-bool)
  vortex-profiles-register-extended-start)

(defvtx* (_fun _VortexCtx-pointer -> _int)
  vortex-profiles-registered)

(defvtx* (_fun _VortexCtx-pointer _string _int -> _void)
  vortex-profiles-set-automatic-mime)

(defvtx* (_fun _VortexCtx-pointer _string _string _string -> _axl-bool)
  vortex-profiles-set-mime-type)

(defvtx* (_fun _VortexCtx-pointer _string _VortexOnFrameReceived _axlPointer -> _axl-bool)
  vortex-profiles-set-received-handler)

(defvtx* (_fun _VortexCtx-pointer _string -> _axl-bool)
  vortex-profiles-unregister)