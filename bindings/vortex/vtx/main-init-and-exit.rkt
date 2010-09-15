#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _VortexCtx-pointer -> _axl-bool)
  vortex-color-log-is-enabled
  vortex-init-check
  vortex-init-ctx
  vortex-log2-is-enabled
  vortex-log-is-enabled
  vortex-log-is-enabled-acquire-mutex)

(defvtx* (_fun _VortexCtx-pointer _axl-bool -> _void)
  vortex-color-log-enable
  vortex-exit-ctx
  vortex-log2-enable
  vortex-log-enable
  vortex-log-acquire-mutex
  vortex-log-set-prepare-log)

(defvtx vortex-conf-get (_fun _VortexCtx-pointer _VortexConfItem 
                              (result : (_ptr o _int)) -> (success? : _axl-bool)
                              -> (values result success?)))

(defvtx vortex-conf-set (_fun _VortexCtx-pointer _int _int _string -> _axl-bool))
(defvtx vortex-log-get-handler (_fun _VortexCtx-pointer -> _VortexLogHandler))
(defvtx vortex-log-set-handler (_fun _VortexCtx-pointer _VortexLogHandler -> _void))