#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _VortexConnection-pointer _string _string _string -> _void) 
  vortex-greetings-error-send) ;; really a ... after the 3rd string (printf-like)

(defvtx* (_fun _VortexCtx-pointer -> _string)
  vortex-greetings-get-features
  vortex-greetings-get-localize)

(defvtx* (_fun _VortexConnection-pointer _VortexConnectionOpts-pointer -> _axl-bool)
  vortex-greetings-send)

(defvtx* (_fun _VortexCtx-pointer _string -> _void)
  vortex-greetings-set-features
  vortex-greetings-set-localize)