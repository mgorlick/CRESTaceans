#lang racket

(require ffi/unsafe)

(define lib (ffi-lib "libracket-pulse-wrapper"))

(define-syntax-rule (defpulse+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) lib typ)))
(define-syntax-rule (defpulse obj typ)
  (defpulse+ obj obj typ))
(define-syntax-rule (defpulse* typ obj ...)
  (begin (defpulse obj typ) ...))


(define-cpointer-type _pulsesrc-pointer)
(defpulse pulsesrc-new (_fun -> _pulsesrc-pointer))