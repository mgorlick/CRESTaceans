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

(define _vorbisenc-process-block (_fun _ogg-packet-pointer _ogg-packet-type -> _bool))

(defvorbis~ vorbisenc-new (_fun -> _vorbisenc-pointer))
(defvorbis~ vorbisenc-delete (_fun _vorbisenc-pointer -> _void))
(defvorbis~ vorbisenc-is-init (_fun _vorbisenc-pointer -> _bool))

(defvorbis~ vorbisenc-init (_fun _vorbisenc-pointer _vorbisenc-process-block -> _int))

(defvorbis~ vorbisenc-encode-pcm-samples
  (_fun (enc samples ct callback) ::
        (enc : _vorbisenc-pointer)
        (samples : (_list io _ubyte ct))
        (ct : _long)
        (callback : _vorbisenc-process-block)
        -> _int))