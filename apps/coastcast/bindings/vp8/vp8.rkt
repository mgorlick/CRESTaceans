#lang racket/base

(require ffi/unsafe
         racket/future
         "../ctypes.rkt")
(provide (except-out (all-defined-out)
                     defvp8
                     defvp8+
                     defvp8*
                     vp8lib))

(define vp8lib (ffi-lib "libracket-vp8-wrapper"))
(define-syntax-rule (defvp8+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) vp8lib typ)))
(define-syntax-rule (defvp8 obj typ)
  (defvp8+ obj obj typ))
(define-syntax-rule (defvp8* typ obj ...)
  (begin (defvp8 obj typ) ...))

(define _quarter-row-enum
  (_enum
   '(top = 0
         bottom = 1)
   _int))
(define _quarter-col-enum
  (_enum
   '(left = 0
          right = 1)
   _int))

;; encoding

(define-cpointer-type _vp8enc-pointer)

(defvp8 vp8enc-new (_fun (_int = (processor-count)) _int _int _int _int -> _vp8enc-pointer))
(defvp8 vp8enc-delete (_fun _vp8enc-pointer -> _void))
(defvp8 vp8enc-encode (_fun _vp8enc-pointer
                            _size_t _bytes
                            _size_t _bytes
                            (written : (_ptr o _size_t))
                            -> (r : _bool)
                            -> (and r written)))
(defvp8 vp8enc-encode-quarter (_fun _vp8enc-pointer _quarter-row-enum _quarter-col-enum
                                    _size_t _bytes
                                    _size_t _bytes
                                    (written : (_ptr o _size_t))
                                    -> (r : _bool)
                                    -> (and r written)))

;; decoding

(defvp8 vp8dec-get-sizes (_fun (process-format-bpp : (_ptr o _float))
                               (display-format-bpp : (_ptr o _float))
                               -> _void
                               -> (values process-format-bpp display-format-bpp)))

(define-values (PROCESS-FORMAT-BPP DISPLAY-FORMAT-BPP) (vp8dec-get-sizes))

(define-cpointer-type _vp8dec-pointer)

(defvp8 vp8dec-new (_fun -> _vp8dec-pointer))
(defvp8 vp8dec-delete (_fun _vp8dec-pointer -> _void))

(define (number->integer n) (inexact->exact (round n)))

(defvp8 vp8dec-decode-copy (_fun (decoder compressed-frame width height) ::
                                 (decoder : _vp8dec-pointer)
                                 (input-size : _size_t = (bytes-length compressed-frame))
                                 (compressed-frame : _bytes)
                                 (output-size : _size_t = (number->integer (* width height PROCESS-FORMAT-BPP)))
                                 (output-frame : _bytes = (make-bytes output-size))
                                 -> (succeeded? : _bool)
                                 -> (if succeeded?
                                        output-frame
                                        #f)))

(define-cpointer-type _color-converter-pointer)

(defvp8 color-converter-new (_fun _uint _uint -> _color-converter-pointer))
(defvp8 color-converter-delete (_fun _color-converter-pointer -> _void))

(defvp8 color-converter-sz (_fun (c) ::
                                 (c : _color-converter-pointer)
                                 (w : (_ptr o _uint))
                                 (h : (_ptr o _uint))
                                 -> _void
                                 -> (* w h)))

(defvp8 yuv420p-to-rgb32 (_fun (c raw-frame) ::
                               (c : _color-converter-pointer)
                               (input-size : _size_t = (bytes-length raw-frame))
                               (raw-frame : _bytes)
                               (output-size : _size_t = (number->integer (* (color-converter-sz c) DISPLAY-FORMAT-BPP)))
                               (output-frame : _bytes = (make-bytes output-size))
                               -> (succeeded? : _bool)
                               -> (if succeeded?
                                      output-frame
                                      #f)))