#lang racket/base

(require ffi/unsafe
         "../ctypes.rkt")
(provide (except-out (all-defined-out)
                     defvorbis~
                     defvorbis~+
                     defvorbis~*
                     libvorbis/adds
                     init))

#|(define libvorbis (ffi-lib "libvorbis"))
(define-syntax-rule (defvorbis+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) libvorbis typ)))
(define-syntax-rule (defvorbis obj typ)
  (defvorbis+ obj obj typ))
(define-syntax-rule (defvorbis* typ obj ...)
  (begin (defvorbis obj typ)
         ...))|#

(define libvorbis/adds (ffi-lib "libracket-vorbis-wrapper"))
(define-syntax-rule (defvorbis~+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) libvorbis/adds typ)))
(define-syntax-rule (defvorbis~ obj typ)
  (defvorbis~+ obj obj typ))
(define-syntax-rule (defvorbis~* typ obj ...)
  (begin (defvorbis~ obj typ) ...))

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

(defvorbis~ init (_fun -> _void))
(init)

;;; additions for building decoder

(define-cpointer-type _vorbisdec-pointer)

(defvorbis~ vorbisdec-new (_fun -> _vorbisdec-pointer))
(defvorbis~ vorbisdec-delete (_fun _vorbisdec-pointer -> _void))
(defvorbis~ vorbisdec-is-init (_fun _vorbisdec-pointer -> _bool))

(defvorbis~ header-packet-in
  (_fun (dec buff len) ::
        (dec : _vorbisdec-pointer)
        ((_list io _ubyte len) = (bytes->list buff))
        (len : _long)
        -> _int))

(defvorbis~ data-packet-blockin
  (_fun (dec buff len) ::
        (dec : _vorbisdec-pointer)
        ((_list io _ubyte len) = (bytes->list buff))
        (len : _long)
        -> _int))

(defvorbis~ stream-channels (_fun _vorbisdec-pointer -> _int))
(defvorbis~ stream-rate (_fun _vorbisdec-pointer -> _int))

;;; ogg packet helpers

(define-cpointer-type _ogg-packet-pointer)
(define-cpointer-type _ogg-demux-pointer)
(define _ogg-packet-for-each (_fun _ogg-packet-pointer -> _void))
(defvorbis~ ogg-packet-copy (_fun _ogg-packet-pointer -> _ogg-packet-pointer))
(defvorbis~ ogg-packet-size (_fun _ogg-packet-pointer -> _long))
(defvorbis~ ogg-packet-data (_fun (p : _ogg-packet-pointer) -> (_bytes o (ogg-packet-size p))))
(defvorbis~ ogg-demux-new (_fun -> _ogg-demux-pointer))
(defvorbis~ ogg-demux-delete (_fun _ogg-demux-pointer -> _void))
(defvorbis~ ogg-demux-data (_fun _ogg-demux-pointer _size_t _bytes _ogg-packet-for-each -> _bool))

;;; additions for building encoder

(define-cpointer-type _vorbisenc-pointer)
(define _ogg-packet-type
  (_enum
   '(id
     comment
     codebook
     data)))

(define _conversion-type
  (_enum
   '(naive
     ntoh)))

(define _vorbisenc-process-block (_fun _ogg-packet-pointer _ogg-packet-type -> _bool))

(defvorbis~ vorbisenc-delete (_fun _vorbisenc-pointer -> _void))
(defvorbis~ vorbisenc-is-init (_fun _vorbisenc-pointer -> _bool))

(defvorbis~ vorbisenc-init (_fun _int _int _float _vorbisenc-process-block -> _vorbisenc-pointer))

(defvorbis~ vorbisenc-encode-pcm-samples
  (_fun (enc buffer size conv callback) ::
        (enc : _vorbisenc-pointer)
        (buffer : _bytes)
        (size : _long)
        (conv : _conversion-type)
        (callback : _vorbisenc-process-block)
        -> _bool))