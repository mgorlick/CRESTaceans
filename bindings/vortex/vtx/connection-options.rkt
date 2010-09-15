#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx vortex-connection-opts-free
  (_fun _VortexConnectionOpts-pointer -> _void))

(defvtx vortex-connection-opts-new
  (_fun _VortexConnectionOptItem -> _VortexConnectionOpts-pointer))

(define vortex-connection-opts-new-2
  (get-ffi-obj 'vortex_connection_opts_new libvortex
               (_fun _VortexConnectionOptItem _VortexConnectionOptItem
                     -> _VortexConnectionOpts-pointer)))

(define vortex-connection-opts-new-3
  (get-ffi-obj 'vortex_connection_opts_new libvortex
               (_fun _VortexConnectionOptItem _VortexConnectionOptItem
                     _VortexConnectionOptItem
                     -> _VortexConnectionOpts-pointer)))

(define vortex-connection-opts-new-4
  (get-ffi-obj 'vortex_connection_opts_new libvortex
               (_fun _VortexConnectionOptItem _VortexConnectionOptItem
                     _VortexConnectionOptItem _VortexConnectionOptItem
                     -> _VortexConnectionOpts-pointer)))