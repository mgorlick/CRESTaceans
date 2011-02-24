#lang racket

(require ffi/unsafe
         "oggpack.rkt")
(provide (all-defined-out)
         (all-from-out "oggpack.rkt"))

(define libvorbis (ffi-lib "libvorbis"))
(define libvorbis/adds (ffi-lib "libracket-vorbis-adds" "1.0"))

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

(define-cstruct _alloc-chain
  ([ptr _pointer] ; XXX problematic
   [next _alloc-chain-pointer]))

(define-cstruct _vorbis-info
  ([version _int]
   [channels _int]
   [rate _long]
   [bitrate-upper _long]
   [bitrate-nominal _long]
   [bitrate-lower _long]
   [bitrate-window _long]
   [codec-setup _pointer])) ; XXX problematic

(define-cstruct _vorbis-dsp-state
  ([analysisp _int]
   [vi _vorbis-info-pointer]
   [pcm (_ptr io (_ptr io _float))]
   [pcmret (_ptr io (_ptr io _float))]
   [pcm-storage _int]
   [pcm-current _int]
   [pcm-returned _int]
   [preextrapolate _int]
   [eofflag _int]
   [lW _long]
   [W _long]
   [nW _long]
   [centerW _long]
   [granulepos _ogg_int64]
   [sequence _ogg_int64]
   [glue-bits _ogg_int64]
   [time-bits _ogg_int64]
   [floor-bits _ogg_int64]
   [res-bits _ogg_int64]
   [backend-state _pointer])) ; XXX problematic

(define-cstruct _vorbis-block
  ([pcm (_ptr io (_ptr io _float))]
   [opb _oggpack-buffer]
   [lW _long]
   [W _long]
   [nW _long]
   [pcmend _int]
   [mode _int]
   [eofflag _int]
   [granulepos _ogg_int64]
   [sequence _ogg_int64]
   [vd _vorbis-dsp-state-pointer]
   [localstore _pointer] ; XXX problematic
   [localtop _long]
   [localalloc _long]
   [totaluse _long]
   [reap _alloc-chain-pointer]
   [glue-bits _long]
   [time-bits _long]
   [floor-bits _long]
   [res-bits _long]
   [internal _pointer])) ; XXX problematic

(define-cstruct _vorbis-comment
  ([user-comments (_ptr io _string)]
   [comment-lengths (_ptr io _int)]
   [comments _int]
   [vendor _string]))

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

;; codec.h
;; general
(defvorbis* (_fun _vorbis-info-pointer -> _void)
  vorbis-info-init
  vorbis-info-clear)
(defvorbis+ vorbis-info-new vorbis-info-init
  (_fun (i : (_ptr o _vorbis-info))
        -> _void
        -> i))

(defvorbis vorbis-block-init (_fun _vorbis-dsp-state-pointer
                                   _vorbis-block-pointer -> _int))
(defvorbis vorbis-block-clear (_fun _vorbis-block-pointer -> _int))
(defvorbis+ vorbis-block-new vorbis-block-init
  (_fun _vorbis-dsp-state-pointer (b : (_ptr o _vorbis-block))
        -> (r : _int)
        -> (if (= r 0) b r)))

(defvorbis* (_fun _vorbis-comment-pointer -> _void)
  vorbis-comment-init
  vorbis-comment-clear)
(defvorbis+ vorbis-comment-new vorbis-comment-init
  (_fun (c : (_ptr o _vorbis-comment))
        -> _void
        -> c))

;; synthesis layer
(defvorbis vorbis-synthesis-idheader (_fun _ogg-packet-pointer -> _int))
(defvorbis vorbis-synthesis-headerin
  (_fun _vorbis-info-pointer _vorbis-comment-pointer _ogg-packet-pointer -> _int))
(defvorbis vorbis-synthesis-init
  (_fun _vorbis-dsp-state-pointer _vorbis-info-pointer -> _int))
(defvorbis vorbis-synthesis-restart (_fun _vorbis-dsp-state-pointer -> _int))
(defvorbis vorbis-synthesis-blockin
  (_fun _vorbis-dsp-state-pointer _vorbis-block-pointer -> _int))
(defvorbis vorbis-synthesis-read (_fun _vorbis-dsp-state-pointer _int -> _int))
(defvorbis vorbis-packet-blocksize (_fun _vorbis-info-pointer _ogg-packet-pointer -> _long))
(defvorbis vorbis-synthesis-halfrate (_fun _vorbis-info-pointer _int -> _int))
(defvorbis vorbis-synthesis-halfrate-p (_fun _vorbis-info-pointer -> _int))

(defvorbis+ vorbis-dsp-state-new vorbis-synthesis-init
  (_fun (d : (_ptr o _vorbis-dsp-state))
        _vorbis-info-pointer
        -> (r : _int)
        -> (if (= r 0) d r)))

(defvorbis* (_fun _vorbis-block-pointer _ogg-packet-pointer -> _int)
  vorbis-synthesis
  vorbis-synthesis-trackonly)
(defvorbis* (_fun _vorbis-dsp-state-pointer (samples : (_ptr o _pointer))
                  -> (r : _int)
                  -> (values r samples))
  vorbis-synthesis-pcmout
  vorbis-synthesis-lapout)

(defvorbis+ vorbis-synthesis-pcmout-countonly vorbis-synthesis-pcmout
  (_fun _vorbis-dsp-state-pointer (samples : _pointer = #f)
        -> _int))

(defvorbis~ copy-buffer-to-packet
  (_fun (packet box bufferlen) :: 
        (packet : _ogg-packet-pointer)
        (box : (_box (_list io _ubyte bufferlen)))
        -> _void))

(defvorbis~ print-buffer
  (_fun _ogg-packet-pointer -> _void))

(defvorbis~ pcmout-wrapper
  (_fun (a b c d) ::
        (a : _vorbis-dsp-state-pointer)
        (b : (_box (_vector io _float c)))
        (d : _int)
        -> _int))