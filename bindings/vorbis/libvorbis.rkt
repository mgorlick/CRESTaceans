#lang racket

(require "oggpack.rkt"
         ffi/unsafe)
(provide (all-from-out "oggpack.rkt")
         (all-defined-out))

(define libvorbis (ffi-lib "libvorbis"))
(define libvorbis/adds (ffi-lib "libracket-vorbis-wrapper"))

(define-syntax-rule (defvorbis+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) libvorbis typ)))
(define-syntax-rule (defvorbis obj typ)
  (defvorbis+ obj obj typ))
(define-syntax-rule (defvorbis* typ obj ...)
  (begin (defvorbis obj typ)
         ...))

(define-syntax-rule (defvorbis~+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) libvorbis/adds typ)))
(define-syntax-rule (defvorbis~ obj typ)
  (defvorbis~+ obj obj typ))
(define-syntax-rule (defvorbis~* typ obj ...)
  (begin (defvorbis~ obj typ)
         ...))

(define OV_FALSE -1)
(define OV_EOF -2)
(define OV_HOLE -3)
(define OV_EREAD -128)
(define OV_EFAULT -129)
(define OV_EIMPL -130)
(define OV_EINVAL -131)
(define OV_ENOTVORBIS -132)
(define OV_EBADHEADER -133)
(define OV_EVERSION -134)
(define OV_ENOTAUDIO -135)
(define OV_EBADPACKET -136)
(define OV_EBADLINK -137)
(define OV_ENOSEEK -138)

;;; additions for building decoder

(define-cpointer-type _vorbisdec-pointer)

(defvorbis~ vorbisdec-new (_fun -> _vorbisdec-pointer))
(defvorbis~ vorbisdec-delete (_fun _vorbisdec-pointer -> _void))
(defvorbis~ vorbisdec-is-init (_fun _vorbisdec-pointer -> _bool))

(defvorbis~ header-packet-in
  (_fun (dec buff len) ::
        (dec : _vorbisdec-pointer)
        (buff : (_box (_list io _ubyte len)))
        (len : _long)
        -> _int))

(defvorbis~ data-packet-blockin
  (_fun (dec c d) ::
        (dec : _vorbisdec-pointer)
        (c : (_box (_list io _ubyte d)))
        (d : _long)
        -> _int))

(defvorbis~ data-packet-pcmout
  (_fun (dec c d) ::
        (dec : _vorbisdec-pointer)
        (c : (_box (_list io _int16 d)))
        -> _int))

(defvorbis~ stream-channels (_fun _vorbisdec-pointer -> _int))
(defvorbis~ stream-rate (_fun _vorbisdec-pointer -> _int))

(define (bytestring->uchar** buffer)
  (box (bytes->list buffer)))