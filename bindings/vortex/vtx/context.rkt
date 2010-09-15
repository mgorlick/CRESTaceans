#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _VortexCtx-pointer -> _void)
  vortex-ctx-free
  vortex-ctx-ref)

(defvtx* (_fun _VortexCtx-pointer _axlPointer -> _axlPointer)
  vortex-ctx-get-data)

(defvtx* (_fun _VortexCtx-pointer _axlDestroyFunc -> _void)
  vortex-ctx-install-cleanup)

(defvtx* (_fun _VortexCtx-pointer _axl-bool -> _void)
  vortex-ctx-server-name-acquire)

(defvtx* (_fun _VortexCtx-pointer _VortexConnectionOnChannelUpdate _axlPointer -> _void)
  vortex-ctx-set-channel-added-handler
  vortex-ctx-set-channel-removed-handler)

(defvtx* (_fun _VortexCtx-pointer _VortexOnStartChannelExtended _axlPointer -> _void)
  vortex-ctx-set-channel-start-handler)

(defvtx* (_fun _VortexCtx-pointer _VortexOnNotifyCloseChannel _axlPointer -> _void)
  vortex-ctx-set-close-notify-handler)

(defvtx* (_fun _VortexCtx-pointer _axlPointer _axlPointer -> _void)
  vortex-ctx-set-data)

(defvtx* (_fun _VortexCtx-pointer _axlPointer _axlPointer _axlDestroyFunc _axlDestroyFunc -> _void)
  vortex-ctx-set-data-full)

(defvtx* (_fun _VortexCtx-pointer _VortexOnFrameReceived _axlPointer -> _void)
  vortex-ctx-set-frame-received)

(defvtx* (_fun _VortexCtx-pointer _VortexOnFinishHandler _axlPointer -> _void)
  vortex-ctx-set-on-finish)

(defvtx* (_fun (_ptr io _VortexCtx-pointer) -> _void)
  vortex-ctx-unref)