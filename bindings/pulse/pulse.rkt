#lang racket

(require ffi/unsafe
         "../ctypes.rkt")
(provide (all-defined-out))

(define lib (ffi-lib "libracket-pulse-wrapper"))

(define-syntax-rule (defpulse+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) lib typ)))
(define-syntax-rule (defpulse obj typ)
  (defpulse+ obj obj typ))
(define-syntax-rule (defpulse* typ obj ...)
  (begin (defpulse obj typ) ...))

(define-cpointer-type _pulsesrc-pointer)
(defpulse pulsesrc-new (_fun -> _pulsesrc-pointer))
(defpulse pulsesrc-delete (_fun _pulsesrc-pointer -> _void))
(defpulse pulsesrc-read (_fun _pulsesrc-pointer
                              _size_t _bytes (_ptr o _size_t)
                              -> _bool))