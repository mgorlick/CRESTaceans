#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx vortex-connection-opts-free
  (_fun _VortexConnectionOpts-pointer -> _void))
(defvtx vortex-connection-opts-new
  (_fun _VortexConnectionOptItem -> _VortexConnectionOpts-pointer))