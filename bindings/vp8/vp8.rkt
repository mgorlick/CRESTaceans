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

(define-cpointer-type _vp8enc-pointer)
(define _vp8enc-each-frame (_fun (size buffer) :: (size : _long) (buffer : (_bytes o size)) -> _void))
(defvp8 vp8enc-new (_fun -> _vp8enc-pointer))
(defvp8 vp8enc-encode (_fun _vp8enc-pointer _long _bytes _vp8enc-each-frame -> _bool))

(define-cpointer-type _vp8dec-pointer)
(defvp8 vp8dec-new (_fun -> _vp8dec-pointer))
(defvp8 vp8dec-decode (_fun _vp8dec-pointer _long _bytes -> _bool))