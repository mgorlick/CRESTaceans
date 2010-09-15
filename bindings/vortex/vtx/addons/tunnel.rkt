#lang racket

(require ffi/unsafe
         "../libvortex.rkt")
(provide (all-defined-out))

(defvtxtu* (_fun _VortexCtx-pointer _VortexOnAcceptedConnection _axlPointer -> _axl-bool)
  vortex-tunnel-accept-negotiation)

(defvtxtu* (_fun _VortexTunnelSettings-pointer _VortexConnectionNew
                 _axlPointer -> _VortexConnection-pointer)
  vortex-tunnel-new)

(defvtxtu* (_fun _VortexCtx-pointer _VortexTunnelLocationResolver _axlPointer -> _void)
  vortex-tunnel-set-resolver)

(defvtxtu* (_fun _VortexTunnelSettings-pointer -> _void) ; XXX
  vortex-tunnel-settings-add-hop)

(defvtxtu* (_fun _VortexTunnelSettings-pointer -> _void)
  vortex-tunnel-settings-free)

(defvtxtu* (_fun _VortexCtx-pointer -> _VortexTunnelSettings-pointer)
  vortex-tunnel-settings-new)

(defvtxtu* (_fun _VortexCtx-pointer _string _int -> _VortexTunnelSettings-pointer)
  vortex-tunnel-settings-new-from-xml)

(defvtxtu* (_fun _VortexTunnelSettings-pointer _VortexConnectionOpts-pointer -> _void)
  vortex-tunnel-settings-set-options)