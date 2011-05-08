#lang racket

(require ffi/unsafe)
(provide (except-out (all-defined-out)
                     defvp8
                     defvp8+
                     defvp8*))

(define lib (ffi-lib "libracket-vp8-wrapper"))
(define-syntax-rule (defvp8+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) lib typ)))
(define-syntax-rule (defvp8 obj typ)
  (defvp8+ obj obj typ))
(define-syntax-rule (defvp8* typ obj ...)
  (begin (defvp8 obj typ) ...))

(define _size_t _uint32)

(define-cpointer-type _vp8enc-pointer)

(defvp8 vp8enc-new (_fun -> _vp8enc-pointer))
(defvp8 vp8enc-encode (_fun _vp8enc-pointer _size_t _bytes _bytes (written : (_ptr o _size_t))
                            -> (r : _bool)
                            -> (if r written #f)))

(defvp8 peek (_fun _size_t _bytes -> _void))

(define-cpointer-type _vp8dec-pointer)
(defvp8 vp8dec-new (_fun -> _vp8dec-pointer))
(defvp8 vp8dec-decode (_fun _vp8dec-pointer _size_t _bytes -> _bool))