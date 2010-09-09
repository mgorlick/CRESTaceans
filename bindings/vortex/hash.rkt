#lang racket

(require "libvortex.rkt"
         ffi/unsafe)
(provide (all-defined-out))

(defvtx* (_fun _VortexHash-pointer -> _void)
  vortex-hash-clear
  vortex-hash-destroy
  vortex-hash-ref
  vortex-hash-unref)

(defvtx* (_fun _VortexHash-pointer _axlPointer -> _axl-bool)
  vortex-hash-delete
  vortex-hash-remove)

(defvtx* (_fun _VortexHash-pointer _axlHashForeachFunc _axlPointer -> _void)
  vortex-hash-foreach)

(defvtx* (_fun _VortexHash-pointer _axlHashForeachFunc2 _axlPointer _axlPointer -> _void)
  vortex-hash-foreach2)

(defvtx* (_fun _VortexHash-pointer _axlHashForeachFunc3 _axlPointer _axlPointer _axlPointer -> _void)
  vortex-hash-foreach3)

(defvtx* (_fun _VortexHash-pointer _axlPointer _axlPointer -> _void)
  vortex-hash-insert
  vortex-hash-replace)

(defvtx* (_fun _VortexHash-pointer _long -> _int)
  vortex-hash-lock-until-changed)

(defvtx* (_fun _VortexHash-pointer _axlPointer -> _axlPointer)
  vortex-hash-lookup
  vortex-hash-lookup-and-clear)

(defvtx* (_fun _axlHashFunc _axlEqualFunc -> _VortexHash-pointer)
  vortex-hash-new)

(defvtx* (_fun _axlHashFunc _axlEqualFunc _axlDestroyFunc _axlDestroyFunc -> _VortexHash-pointer)
  vortex-hash-new-full)

(defvtx* (_fun _VortexHash-pointer _axlPointer _axlDestroyFunc _axlPointer _axlDestroyFunc -> _void)
  vortex-hash-replace-full)

(defvtx* (_fun _VortexHash-pointer -> _int)
  vortex-hash-size)