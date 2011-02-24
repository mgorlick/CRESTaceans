#lang racket

(require ffi/unsafe)
(provide (all-defined-out))

(define _ogg_int64 _int64)
(define ogg64size (ctype-sizeof _ogg_int64))
(define longsize (ctype-sizeof _long))

(define-cstruct _oggpack-buffer
  ([endbyte _long]
   [endbit _long]
   [buffer _string]
   [ptr _string]
   [storage _long]))

(define-cstruct _ogg-packet
  ([packet _pointer]
   [bytes _long]
   [b-o-s _long]
   [e-o-s _long]
   [granulepos _ogg_int64]
   [packetno _ogg_int64]))