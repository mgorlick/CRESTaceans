#lang racket

(require ffi/unsafe
         "../vorbis/libvorbis.rkt")

(provide (all-defined-out)
         ogg-packet-data)

(define theora (ffi-lib "libracket-theora-wrapper"))
(define-syntax-rule (deftheora+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) theora typ)))
(define-syntax-rule (deftheora obj typ)
  (deftheora+ obj obj typ))
(define-syntax-rule (deftheora* typ obj ...)
  (begin (deftheora obj typ) ...))

(define-cpointer-type _theoradec-pointer)
(define-cpointer-type _theoraenc-pointer)

(define _theoraenc-each-packet (_fun _ogg-packet-pointer -> _bool))

;; need to do some explicit data representation to make it easier on ourselves
;; when we get to the encoding part

(define _th-pixel-fmt
  (_enum '(th-pf-420
           th-pf-rsvd
           th-pf-422
           th-pf-444
           th-pf-nformats)))

(define _th-colorspace
  (_enum '(th-cs-unspecified
           th-cs-itu-rec-470m
           th-cs-itu-rec-470bg
           th-cs-nspaces)))

(define-cstruct _th-info
  ([frame-width _uint32]
   [frame-height _uint32]
   [pic-width _uint32]
   [pic-height _uint32]
   [pic-x _uint32]
   [pic-y _uint32]
   [colorspace _th-colorspace]
   [pixel-fmt _th-pixel-fmt]
   [target-bitrate _int]
   [quality _int]
   [keyframe-granule-shift _int]
   [version-major _ubyte]
   [version-minor _ubyte]
   [version-subminor _ubyte]
   [fps-numerator _uint32]
   [fps-denominator _uint32]
   [aspect-numerator _uint32]
   [aspect-denominator _uint32]))

(define-cstruct _th-image-plane
  ([width _int]
   [height _int]
   [stride _int]
   [data _pointer]))

(define _ycbcr-buffer (_list o _th-image-plane 3))

;;; decoding stuff

(deftheora theoradec-new (_fun -> _theoradec-pointer))
(deftheora theoradec-delete (_fun _theoradec-pointer -> _void))

(deftheora theoradec-header-in (_fun (dec buff) ::
                                     (dec : _theoradec-pointer)
                                     (buff : _bytes)
                                     (_long = (bytes-length buff))
                                     -> _bool))

(deftheora theoradec-data-in (_fun (dec ibuff obuff obl) ::
                                   (dec : _theoradec-pointer)
                                   (ibuff : _bytes)
                                   (_long = (bytes-length ibuff))
                                   (obuff : (_box (_list io _ubyte obl)))
                                   (obl : _long)
                                   -> _bool))

;;; encoding stuff

(deftheora theoraenc-new (_fun -> _theoraenc-pointer))
(deftheora theoraenc-delete (_fun _theoraenc-pointer -> _void))

(deftheora theoraenc-foreach-header (_fun _theoraenc-pointer
                                          _theoraenc-each-packet -> _bool))

(deftheora theoraenc-data-in (_fun _theoraenc-pointer 
                                   _bytes _long
                                   _theoraenc-each-packet -> _bool))

;; video capture

(define-cpointer-type _v4l2-reader-pointer)

(deftheora v4l2-reader-setup (_fun -> _v4l2-reader-pointer))

(deftheora v4l2-reader-delete (_fun _v4l2-reader-pointer -> _void))

(deftheora v4l2-reader-get-params
  (_fun _v4l2-reader-pointer
        (frame-width : (_ptr o _int))
        (frame-height : (_ptr o _int))
        (fps-num : (_ptr o _int))
        (fps-denom : (_ptr o _int))
        (buffer-ct : (_ptr o _int))
        -> _void
        -> (values frame-width frame-height fps-num fps-denom buffer-ct)))

;; get a valid pointer to the memory mapped bytestring with its size
;; and index number tracked. mmapped buffer is not requeued until
;; v4l2-reader-enqueue-buffer is called with the index number returned 
;; by v4l2-reader-get-frame-data
(deftheora v4l2-reader-get-frame 
  (_fun _v4l2-reader-pointer
        (size : (_ptr o _int))
        (framenum : (_ptr o _int))
        (index : (_ptr o _int))
        -> (r : _pointer)
        -> (values (cast r _pointer (_bytes o size)) framenum index)))

;; requeue the buffer into the memory mapping queue once the downstream
;; consumers are done with its data
(deftheora v4l2-reader-enqueue-buffer
  (_fun _v4l2-reader-pointer _int -> _bool))