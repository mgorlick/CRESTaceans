#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _Vortex-Socket -> _Vortex-Socket)
  vortex-listener-accept)

(defvtx* (_fun _VortexConnection-pointer _axl-bool -> _void)
  vortex-listener-accept-connection
  vortex-listener-send-greetings-on-connect)

(defvtx* (_fun _VortexCtx-pointer -> _string)
  vortex-listener-get-default-realm)

(defvtx* (_fun _VortexCtx-pointer _string _string 
               _VortexListenerReady _axlPointer -> _VortexConnection-pointer)
  vortex-listener-new)

(defvtx* (_fun _VortexCtx-pointer _string _int
               _VortexListenerReady _axlPointer -> _VortexConnection-pointer)
  vortex-listener-new2)

(defvtx* (_fun _VortexCtx-pointer _string _string 
               _VortexListenerReadyFull _axlPointer -> _VortexConnection-pointer)
  vortex-listener-new-full)

(defvtx* (_fun _VortexCtx-pointer -> _axl-bool)
  vortex-listener-parse-conf-and-start)

(defvtx* (_fun _VortexCtx-pointer _string -> _void)
  vortex-listener-set-default-realm)

(defvtx* (_fun _VortexCtx-pointer _VortexOnAcceptedConnection _axlPointer -> _void)
  vortex-listener-set-on-connection-accepted)

(defvtx* (_fun _VortexConnection-pointer _axl-bool -> _void)
  vortex-listener-shutdown)

(defvtx* (_fun _VortexCtx-pointer -> _void)
  vortex-listener-unlock
  vortex-listener-wait)