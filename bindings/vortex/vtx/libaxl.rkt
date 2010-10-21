#lang racket

(require ffi/unsafe
         (only-in "libvortex-definers.rkt" define-vtx-definer))
(provide (all-defined-out))

(define libaxl (ffi-lib "libaxl"))
(define-vtx-definer libaxl defaxl defaxl*)

; axl types
(define _axl-bool _int)
(define _axlPointer _pointer)
(define _axlDestroyFunc (_fun _axlPointer -> _void))
(define _axlEqualFunc (_fun _axlPointer _axlPointer -> _int))
(define _axlHashFunc (_fun _axlPointer -> _uint))
(define _axlHashForeachFunc (_fun _axlPointer _axlPointer _axlPointer -> _axl-bool))
(define _axlHashForeachFunc2 (_fun _axlPointer _axlPointer _axlPointer
                                   _axlPointer -> _axl-bool))
(define _axlHashForeachFunc3 (_fun _axlPointer _axlPointer
                                   _axlPointer _axlPointer _axlPointer -> _axl-bool))
(define _axlHashForeachFunc4 (_fun _axlPointer _axlPointer _axlPointer
                                   _axlPointer _axlPointer _axlPointer -> _axl-bool))
(define-cpointer-type _axlError-pointer)
(define-cpointer-type _axlList-pointer)
(define-cpointer-type _axlListCursor-pointer)
(define-cpointer-type _axlDoc-pointer)

(define axlError*? (flat-named-contract 'axlError*? axlError-pointer?))

(define axl-false 0)
(define axl-true 1)

(define-syntax-rule (vtx-false? v)
  (= v axl-false))

(define-syntax-rule (vtx-true? v)
  (= v axl-true))

(defaxl* (_fun -> _void)
  axl-init)

;;;;; lists 

(defaxl* (_fun _axlPointer _axlPointer -> _int)
  axl-list-always-return-1
  axl-list-equal-string
  axl-list-equal-int)
(defaxl* (_fun _axlEqualFunc _axlDestroyFunc -> _axlList-pointer)
  axl-list-new)
(defaxl* (_fun _axlList-pointer _axlPointer -> _void)
  axl-list-add
  axl-list-prepend
  axl-list-append
  axl-list-unlink
  axl-list-remove)
(defaxl* (_fun _axlList-pointer _int -> _void)
  axl-list-add-at
  axl-list-remove-at
  axl-list-unlink-at)
(defaxl* (_fun _axlList-pointer -> _void)
  axl-list-remove-first
  axl-list-remove-last
  axl-list-unlink-first
  axl-list-unlink-last
  axl-list-free)
(defaxl* (_fun _axlList-pointer _axlPointer -> _axl-bool)
  axl-list-exists)
(defaxl* (_fun _axlList-pointer _axlPointer _int -> _axl-bool)
  axl-list-exists-at)
(defaxl* (_fun _axlList-pointer -> _axl-bool)
  axl-list-is-empty)
(defaxl* (_fun _axlList-pointer -> _axlPointer)
  axl-list-get-first
  axl-list-get-last)
(defaxl* (_fun _axlList-pointer _int -> _axlPointer)
  axl-list-get-nth)
(defaxl* (_fun _axlList-pointer -> _int)
  axl-list-length)

;;;;; list cursor

(defaxl* (_fun _axlListCursor-pointer -> _void)
  axl-list-cursor-first
  axl-list-cursor-last
  axl-list-cursor-next
  axl-list-cursor-previous
  axl-list-cursor-unlink
  axl-list-cursor-remove)
(defaxl* (_fun _axlListCursor-pointer -> _axl-bool)
  axl-list-cursor-has-next
  axl-list-cursor-has-previous
  axl-list-cursor-has-item)
(defaxl* (_fun _axlList-pointer -> _axlListCursor-pointer)
  axl-list-cursor-new)
(defaxl* (_fun _axlListCursor-pointer -> _axlList-pointer)
  axl-list-cursor-list)
(defaxl* (_fun _axlListCursor-pointer -> _axlPointer)
  axl-list-cursor-get)