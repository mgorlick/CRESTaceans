#lang racket

(require ffi/unsafe
         "../libvortex.rkt")
(provide (all-defined-out))

(define _VortexEventType
  (_enum '(unknown = 0
                   frame-received = 1
                   channel-close = 2
                   channel-added = 4
                   channel-removed = 8
                   connection-closed = 16
                   connection-accepted = 32
                   channel-start = 64)))


(define-cpointer-type _VortexEvent-pointer)
(define-cpointer-type _VortexEventMask-pointer)

(defvtxp* (_fun _VortexEvent-pointer -> _VortexChannel-pointer)
  vortex-event-get-channel)

(defvtxp* (_fun _VortexEvent-pointer -> _VortexConnection-pointer)
  vortex-event-get-conn)

(defvtxp* (_fun _VortexEvent-pointer -> _VortexCtx-pointer)
  vortex-event-get-ctx)

(defvtxp* (_fun _VortexEvent-pointer -> _VortexEncoding) 
  vortex-event-get-encoding)

(defvtxp* (_fun _VortexEvent-pointer -> _VortexFrame-pointer)
  vortex-event-get-frame)

(defvtxp* (_fun _VortexEvent-pointer -> _int)
  vortex-event-get-msgno)

(defvtxp* (_fun _VortexEvent-pointer -> _string)
  vortex-event-get-profile-content
  vortex-event-get-server-name)

(defvtxp* (_fun _VortexEvent-pointer -> _VortexEventType)
  vortex-event-get-type)

(defvtxp* (_fun _VortexEventMask-pointer _int -> _void)
  vortex-event-mask-add)

(defvtxp* (_fun _VortexEventMask-pointer _axl-bool -> _void)
  vortex-event-mask-enable)

(defvtxp* (_fun _VortexEventMask-pointer -> _void)
  vortex-event-mask-free)

(defvtxp* (_fun _VortexEventMask-pointer _VortexEventType -> _axl-bool)
  vortex-event-mask-is-set)

(defvtxp* (_fun _string _int _axl-bool -> _VortexEventMask-pointer)
  vortex-event-mask-new)

(defvtxp* (_fun _VortexEventMask-pointer _int -> _void)
  vortex-event-mask-remove)

(defvtxp* (_fun _VortexEvent-pointer -> _axl-bool)
  vortex-event-ref)

(defvtxp* (_fun _VortexEvent-pointer -> _void)
  vortex-event-unref)

(defvtxp* (_fun _VortexCtx-pointer -> _void)
  vortex-pull-cleanup)

(defvtxp* (_fun _VortexCtx-pointer -> _axl-bool)
  vortex-pull-init
  vortex-pull-pending-events)

(defvtxp* (_fun _VortexCtx-pointer _int -> _VortexEvent-pointer)
  vortex-pull-next-event)

(defvtxp* (_fun _VortexCtx-pointer -> _int)
  vortex-pull-pending-events-num)

(defvtxp* (_fun _VortexCtx-pointer _VortexEventMask-pointer
                (error : (_ptr o _axlError-pointer)) -> (result : _axl-bool)
                -> (values result error))
  vortex-pull-set-event-mask)