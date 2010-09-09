#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _VortexCtx-pointer _string _string -> _void)
  vortex-support-add-domain-search-path
  vortex-support-add-domain-search-path-ref)

(defvtx* (_fun _VortexCtx-pointer _string -> _void)
  vortex-support-add-search-path
  vortex-support-add-search-path-ref)

(defvtx* (_fun _VortexCtx-pointer _string _string -> _axl-bool)
  vortex-support-check-search-path)

(defvtx* (_fun _VortexCtx-pointer -> _void)
  vortex-support-cleanup
  vortex-support-init)

(defvtx* (_fun _VortexCtx-pointer _string _string -> _string)
  vortex-support-domain-find-data-file)

(defvtx* (_fun _string _VortexFileTest -> _axl-bool)
  vortex-support-file-test)

(defvtx* (_fun _VortexCtx-pointer _string -> _string)
  vortex-support-find-data-file)

(defvtx* (_fun _VortexCtx-pointer _sockaddr-in-pointer -> _string)
  vortex-support-inet-ntoa)

(defvtx* (_fun _string _string -> _axl-bool)
  vortex-support-setenv)

(defvtx* (_fun _string (_ptr io _string) -> _double)
  vortex-support-strtod)

(defvtx* (_fun _string -> _axl-bool)
  vortex-support-unsetenv)

(defvtx* (_fun _timeval-pointer _timeval-pointer _timeval-pointer -> _int)
  vortex-timeval-substract)