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

(define OUTPUT-FORMAT-BPP 4)

(define-cpointer-type _vp8dec-pointer)

(defvp8 vp8dec-new (_fun -> _vp8dec-pointer))
(defvp8 vp8dec-delete (_fun _vp8dec-pointer -> _void))

(defvp8 vp8dec-decode-copy (_fun (decoder compressed-frame width height) ::
                                 (decoder : _vp8dec-pointer)
                                 (input-size : _size_t = (bytes-length compressed-frame))
                                 (compressed-frame : _bytes)
                                 (output-size : _size_t = (* OUTPUT-FORMAT-BPP width height))
                                 (output-frame : _bytes = (make-bytes output-size))
                                 -> (succeeded? : _bool)
                                 -> (if succeeded?
                                        output-frame
                                        #f)))

; vp8dec-pointer? bytes? exact-nonnegative-integer? exact-nonnegative-integer? bytes? -> (or/c bytes? #f))
(defvp8 vp8dec-decode-update-minor (_fun (decoder-major decoder-minor compressed-frame old-output-frame) ::
                                         (decoder-major : _vp8dec-pointer)
                                         (decoder-minor : _vp8dec-pointer)
                                         (input-size : _size_t = (bytes-length compressed-frame))
                                         (compressed-frame : _bytes)
                                         (output-size : _size_t = (bytes-length old-output-frame))
                                         (output-frame : _bytes = (bytes-copy old-output-frame))
                                         -> (succeeded? : _bool)
                                         -> (if succeeded?
                                                output-frame
                                                #f)))

; vp8dec-pointer? bytes? exact-nonnegative-integer? exact-nonnegative-integer? bytes? -> (or/c bytes? #f))
(defvp8 vp8dec-decode-update-major (_fun (decoder-major decoder-minor compressed-frame old-output-frame) ::
                                         (decoder-major : _vp8dec-pointer)
                                         (decoder-minor : _vp8dec-pointer)
                                         (input-size : _size_t = (bytes-length compressed-frame))
                                         (compressed-frame : _bytes)
                                         (output-size : _size_t = (bytes-length old-output-frame))
                                         (output-frame : _bytes = (bytes-copy old-output-frame))
                                         -> (succeeded? : _bool)
                                         -> (if succeeded?
                                                output-frame
                                                #f)))