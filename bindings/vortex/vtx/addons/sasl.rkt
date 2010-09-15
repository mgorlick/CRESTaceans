#lang racket

(require ffi/unsafe
         "../libvortex.rkt")
(provide (all-defined-out))

(define SASL-GSSAPI "http://iana.org/beep/SASL/GSSAPI")
(define SASL-KERBEROS-V4 "http://iana.org/beep/SASL/KERBEROS_V4")
(define SASL-PLAIN "http://iana.org/beep/SASL/PLAIN")
(define SASL-CRAM-MD5 "http://iana.org/beep/SASL/CRAM-MD5")
(define SASL-DIGEST-MD5 "http://iana.org/beep/SASL/DIGEST-MD5")
(define SASL-ANONYMOUS "http://iana.org/beep/SASL/ANONYMOUS")

(define SASL-Anonymous-Token "sasl:anonymous:token")
(define SASL-AuthID "sasl:authid")
(define SASL-is-Authenticated "sasl:is:authenticated")
(define SASL-Method-Used "sasl:method:used")
(define SASL-Realm "sasl:realm")

(define-cpointer-type _VortexSaslProps-pointer)

(define _VortexSaslAuthAnonymous
  (_fun _VortexConnection-pointer _string -> _axl-bool))

(define _VortexSaslAuthAnonymousFull
  (_fun _VortexConnection-pointer _string _axlPointer -> _axl-bool))

(define _VortexSaslAuthCramMd5
  (_fun _VortexConnection-pointer _string _string -> _string))

(define _VortexSaslAuthCramMd5Full
  (_fun _VortexConnection-pointer _string _axlPointer -> _string))

(define _VortexSaslAuthDigestMd5
  (_fun _VortexConnection-pointer _string _string _string -> _string))

(define _VortexSaslAuthDigestMd5Full
  (_fun _VortexConnection-pointer _string _string _string 
        _string _axlPointer -> _string))

(define _VortexSaslAuthExternal
  (_fun _VortexConnection-pointer _string -> _axl-bool))

(define _VortexSaslAuthExternalFull
  (_fun _VortexConnection-pointer _string _axlPointer -> _axl-bool))

(define _VortexSaslAuthNotify
  (_fun _VortexConnection-pointer _VortexStatus _string _axlPointer -> _void))

(define _VortexSaslAuthPlain
  (_fun _VortexConnection-pointer _string _string _string -> _axl-bool))

(define _VortexSaslAuthPlainFull
  (_fun _VortexConnection-pointer _string _string _string _axlPointer -> _axl-bool))

(define _VortexSaslCommonHandler
  (_fun _VortexConnection-pointer _VortexSaslProps-pointer
        _axlPointer -> _axlPointer))

(define _VortexSaslProperties
  (_enum '(sasl-auth-id = 1
                        sasl-authorization-id = 2
                        sasl-password = 3
                        sasl-realm = 4
                        sasl-anonymous-token = 5)))

(defvtxs* (_fun _VortexCtx-pointer _string -> _axl-bool)
  vortex-sasl-accept-negotiation)

(defvtxs* (_fun _VortexCtx-pointer _string
                _VortexSaslCommonHandler _axlPointer -> _axl-bool)
  vortex-sasl-accept-negotiation-common)

(defvtxs* (_fun _VortexCtx-pointer _string _axlPointer -> _axl-bool)
  vortex-sasl-accept-negotiation-full)

(defvtxs* (_fun _VortexCtx-pointer -> _void)
  vortex-sasl-cleanup)

(defvtxs* (_fun _VortexCtx-pointer -> _axl-bool)
  vortex-sasl-init)

(defvtxs* (_fun _VortexCtx-pointer _VortexSaslAuthAnonymous -> _void)
  vortex-sasl-set-anonymous-validation)

(defvtxs* (_fun _VortexCtx-pointer _VortexSaslAuthAnonymousFull -> _void)
  vortex-sasl-set-anonymous-validation-full)

(defvtxs* (_fun _VortexCtx-pointer _VortexSaslAuthCramMd5 -> _void)
  vortex-sasl-set-cram-md5-validation)

(defvtxs* (_fun _VortexCtx-pointer _VortexSaslAuthCramMd5Full -> _void)
  vortex-sasl-set-cram-md5-validation-full)

(defvtxs* (_fun _VortexCtx-pointer _VortexSaslAuthExternal -> _void)
  vortex-sasl-set-external-validation)

(defvtxs* (_fun _VortexCtx-pointer _VortexSaslAuthExternalFull -> _void)
  vortex-sasl-set-external-validation-full)

(defvtxs* (_fun _VortexCtx-pointer _VortexSaslAuthPlain -> _void)
  vortex-sasl-set-plain-validation)

(defvtxs* (_fun _VortexCtx-pointer _VortexSaslAuthPlainFull -> _void)
  vortex-sasl-set-plain-validation-full)

(defvtxs* (_fun _VortexConnection-pointer -> _string)
  vortex-sasl-auth-method-used)

(defvtxs* (_fun _VortexConnection-pointer _VortexSaslProperties -> _string)
  vortex-sasl-get-propertie)

(defvtxs* (_fun _VortexConnection-pointer -> _axl-bool)
  vortex-sasl-is-authenticated)

(defvtxs* (_fun _VortexConnection-pointer _VortexSaslProperties
                _string _axlDestroyFunc -> _axl-bool))

(defvtxs* (_fun _VortexConnection-pointer _string
                _VortexSaslAuthNotify _axlPointer -> _void)
  vortex-sasl-start-auth)

(defvtxs* (_fun _VortexConnection-pointer _string
                _VortexStatus 
                (status-msg : (_ptr o _string)) -> _void
                -> status-msg)
  vortex-sasl-start-auth-sync)