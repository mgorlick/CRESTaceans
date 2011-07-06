#lang racket/base

(require ffi/unsafe
         "../ctypes.rkt")
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

(defvp8 vp8enc-new (_fun _int _int _int _int -> _vp8enc-pointer))
(defvp8 vp8enc-delete (_fun _vp8enc-pointer -> _void))
(defvp8 vp8enc-encode (_fun _vp8enc-pointer
                            _size_t _bytes
                            _size_t _bytes
                            (written : (_ptr o _size_t))
                            -> (r : _bool)
                            -> (and r written)))

(define-cpointer-type _vp8dec-pointer)
(defvp8 vp8dec-new (_fun -> _vp8dec-pointer))
(defvp8 vp8dec-delete (_fun _vp8dec-pointer -> _void))
(defvp8 vp8dec-decode (_fun _vp8dec-pointer _size_t _bytes -> _bool))

;; video capture

(define-cpointer-type _v4l2-reader-pointer)

(defvp8 v4l2-reader-setup (_fun -> _v4l2-reader-pointer))

(defvp8 v4l2-reader-delete (_fun _v4l2-reader-pointer -> _void))

(defvp8 v4l2-reader-get-params
  (_fun _v4l2-reader-pointer
        (frame-width : (_ptr o _int))
        (frame-height : (_ptr o _int))
        (fps-num : (_ptr o _int))
        (fps-denom : (_ptr o _int))
        (buffer-ct : (_ptr o _int))
        -> _void
        -> (values frame-width frame-height fps-num fps-denom buffer-ct)))

(defvp8 v4l2-reader-is-ready
  (_fun _v4l2-reader-pointer -> _bool))

;; get a valid pointer to the memory mapped bytestring with its size
;; and index number tracked. mmapped buffer is not requeued until
;; v4l2-reader-enqueue-buffer is called with the index number returned 
;; by v4l2-reader-get-frame-data
(defvp8 v4l2-reader-get-frame 
  (_fun _v4l2-reader-pointer
        (size : (_ptr o _int))
        (framenum : (_ptr o _int))
        (index : (_ptr o _int))
        -> (r : _pointer)
        -> (values (cast r _pointer (_bytes o size)) framenum size index)))

;; requeue the buffer into the memory mapping queue once the downstream
;; consumers are done with its data
(defvp8 v4l2-reader-enqueue-buffer
  (_fun _v4l2-reader-pointer _int -> _bool))