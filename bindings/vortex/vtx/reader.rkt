#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _VortexCtx-pointer _VortexConnection-pointer -> _void)
  vortex-reader-watch-listener
  vortex-reader-watch-connection
  vortex-reader-unwatch-connection)

(defvtx* (_fun _VortexCtx-pointer -> _int)
  vortex-reader-connections-watched
  vortex-reader-run
  vortex-reader-notify-change-io-api)

(defvtx* (_fun _VortexCtx-pointer -> _void)
  vortex-reader-stop
  vortex-reader-notify-change-done-io-api)

(defvtx* (_fun _VortexCtx-pointer _VortexConnection-pointer
               _VortexChannel-pointer _VortexFrame-pointer -> _axl-bool)
  vortex-reader-invoke-frame-received)