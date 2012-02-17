#lang racket/base

(require ffi/unsafe
         racket/future
         "../ctypes.rkt")
(provide (except-out (all-defined-out)
                     defvp8
                     defvp8+
                     defvp8*
                     vp8lib
                     v4l2lib))

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

(define-cpointer-type _vp8dec-pointer)

(defvp8 vp8dec-new (_fun -> _vp8dec-pointer))
(defvp8 vp8dec-delete (_fun _vp8dec-pointer -> _void))

(defvp8 vp8dec-decode-copy (_fun (decoder compressed-frame width height) ::
                                 (decoder : _vp8dec-pointer)
                                 (input-size : _size_t = (bytes-length compressed-frame))
                                 (compressed-frame : _bytes)
                                 (output-size : _size_t = (* 3 width height))
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

;; video capture

(define v4l2lib (ffi-lib "libracket-v4l2-wrapper"))
(define-syntax-rule (defv4l2+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) v4l2lib typ)))
(define-syntax-rule (defv4l2 obj typ)
  (defv4l2+ obj obj typ))
(define-syntax-rule (defv4l2* typ obj ...)
  (begin (defv4l2 obj typ) ...))

(define-cpointer-type _v4l2-reader-pointer)

(defv4l2 v4l2-reader-setup (_fun _string _uint _uint -> _v4l2-reader-pointer))

(defv4l2 v4l2-reader-delete (_fun _v4l2-reader-pointer -> _void))

(defv4l2 v4l2-reader-get-params
  (_fun _v4l2-reader-pointer
        (frame-width : (_ptr o _uint))
        (frame-height : (_ptr o _uint))
        (fps-num : (_ptr o _uint))
        (fps-denom : (_ptr o _uint))
        (buffer-ct : (_ptr o _uint))
        -> _void
        -> (values frame-width frame-height fps-num fps-denom buffer-ct)))

(defv4l2 v4l2-reader-is-ready
  (_fun _v4l2-reader-pointer -> _bool))

;; get a valid pointer to the memory mapped bytestring with its size
;; and index number tracked. mmapped buffer is not requeued until
;; v4l2-reader-enqueue-buffer is called with the index number returned 
;; by v4l2-reader-get-frame-data
(defv4l2 v4l2-reader-get-frame 
  (_fun _v4l2-reader-pointer
        (size : (_ptr o _int))
        (framenum : (_ptr o _int))
        (index : (_ptr o _int))
        -> (r : _pointer)
        -> (values (cast r _pointer (_bytes o size)) framenum size index)))

;; requeue the buffer into the memory mapping queue once the downstream
;; consumers are done with its data
(defv4l2 v4l2-reader-enqueue-buffer
  (_fun _v4l2-reader-pointer _int -> _bool))