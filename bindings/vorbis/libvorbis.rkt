#lang racket

(require ffi/unsafe)
(provide (except-out (all-defined-out)
                     defvorbis~
                     defvorbis~+
                     defvorbis~*
                     libvorbis/adds))

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

(defvorbis~ data-packet-pcmout
  (_fun (dec c d) ::
        (dec : _vorbisdec-pointer)
        (c : (_box (_list io _int16 d)))
        -> _int))

(defvorbis~ data-packet-notify-nodata
  (_fun _vorbisdec-pointer -> _int))

(defvorbis~ stream-channels (_fun _vorbisdec-pointer -> _int))
(defvorbis~ stream-rate (_fun _vorbisdec-pointer -> _int))

;;; ogg packet helpers

(define-cpointer-type _ogg-packet-pointer)
(defvorbis~ ogg-packet-copy (_fun _ogg-packet-pointer -> _ogg-packet-pointer))
(defvorbis~ ogg-packet-size (_fun _ogg-packet-pointer -> _long))
(defvorbis~ ogg-packet-data (_fun (p : _ogg-packet-pointer) -> (_bytes o (ogg-packet-size p))))

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
  (_fun (enc buffer conv callback) ::
        (enc : _vorbisenc-pointer)
        (buffer : _bytes)
        (_long = (bytes-length buffer))
        (conv : _conversion-type)
        (callback : _vorbisenc-process-block)
        -> _bool))