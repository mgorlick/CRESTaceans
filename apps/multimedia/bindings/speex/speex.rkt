#lang racket/base

(require ffi/unsafe
         "../ctypes.rkt")

(provide (except-out (all-defined-out) lib))

(define lib (ffi-lib "libracket-speex-wrapper"))
(define-syntax-rule (defspx+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) lib typ)))
(define-syntax-rule (defspx obj typ)
  (defspx+ obj obj typ))
(define-syntax-rule (defspx* typ obj ...)
  (begin (defspx obj typ) ...))

(defspx init (_fun -> _void))
(init)

(define-cpointer-type _SpeexEncoder-pointer)
(defspx new-speex-encoder (_fun _uint8 (framesize : (_ptr o _uint))
                                -> (p : _SpeexEncoder-pointer)
                                -> (vector p framesize)))
(defspx delete-speex-encoder (_fun _SpeexEncoder-pointer -> _void))
(defspx speex-encoder-encode (_fun (p buff) ::
                     (p : _SpeexEncoder-pointer) (_size_t = (bytes-length buff)) (buff : _bytes)
                     -> (amt : _int)))

(define-cpointer-type _SpeexDecoder-pointer)
(defspx new-speex-decoder (_fun _uint -> _SpeexDecoder-pointer))
(defspx delete-speex-decoder (_fun _SpeexDecoder-pointer -> _void))
(defspx speex-decoder-decode (_fun _SpeexDecoder-pointer _size_t _bytes -> _void))

#|(define enc* (new-speex-encoder 3))
(define e (vector-ref enc* 0))
(define framesize (vector-ref enc* 1))
(define d (new-speex-decoder framesize))
(define outbuff (make-bytes 10000))
(let loop ()
  (define available (speex-encoder-encode e outbuff))
  (speex-decoder-decode d available outbuff)
  (loop))|#