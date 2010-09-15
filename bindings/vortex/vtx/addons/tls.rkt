#lang racket

(require ffi/unsafe
         "../libvortex.rkt")
(provide (all-defined-out))

(defvtxtl* (_fun _VortexCtx-pointer _VortexTlsAcceptQuery _VortexTlsCertificateFileLocator
                 _VortexTlsPrivateKeyFileLocator -> _axl-bool)
  vortex-tls-accept-negotiation)

(defvtxtl* (_fun _VortexDigestMethod _string -> _string)
  vortex-tls-get-digest)

(defvtxtl* (_fun _VortexDigestMethod _string _int -> _string)
  vortex-tls-get-digest-sized)

(defvtxtl* (_fun _VortexConnection-pointer _VortexDigestMethod -> _string)
  vortex-tls-get-peer-ssl-digest)

(defvtxtl* (_fun _VortexConnection-pointer -> _axlPointer)
  vortex-tls-get-ssl-object)

(defvtxtl* (_fun _VortexCtx-pointer -> _axl-bool)
  vortex-tls-init)

(defvtxtl* (_fun _VortexCtx-pointer _int _int _string -> _void)
  vortex-tls-set-auto-tls)

(defvtxtl* (_fun _VortexConnection-pointer _SSL-pointer _SSL-CTX-pointer -> _void)
  vortex-tls-set-common-data)

(defvtxtl* (_fun _VortexConnection-pointer _VortexTlsCtxCreation _axlPointer -> _void)
  vortex-tls-set-ctx-creation)

(defvtxtl* (_fun _VortexCtx-pointer _VortexTlsCtxCreation _axlPointer -> _void)
  vortex-tls-set-default-ctx-creation)

(defvtxtl* (_fun _VortexCtx-pointer _VortexTlsPostCheck _axlPointer -> _void)
  vortex-tls-set-default-post-check)

(defvtxtl* (_fun _VortexConnection-pointer _VortexTlsPostCheck _axlPointer -> _void)
  vortex-tls-set-post-check)

(defvtxtl* (_fun _VortexConnection-pointer _string _VortexTlsActivation _axlPointer -> _void)
  vortex-tls-start-negotiation)

(defvtxtl* (_fun _VortexConnection-pointer _string 
                 _VortexStatus (_ptr io _string) -> _VortexConnection-pointer)
  vortex-tls-start-negotiation-sync)