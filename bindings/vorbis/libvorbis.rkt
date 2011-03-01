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

;; codec_internal.h

#|(define-cstruct _private-state
  ([ve _envelope-lookup-pointer]
   [window (_list io _int 2)]
   [transform (_list io (_ptr io (_ptr io _vorbis-look-transform-pointer)) 2)]
   [fft-look (_list io _drft-lookup 2)]
   [modebits _int]
   [flr (_ptr io (_ptr io _vorbis-look-floor))]
   [residue (_ptr io (_ptr io _vorbis-look-residue))]
   [psy (_ptr io _vorbis-look-psy)]
   [psy-g-look (_ptr io _vorbis-look-psy-global)]
   [header _pointer] ; unsigned char* - only on encoding side
   [header1 _pointer] ; unsigned char* - only on encoding side
   [header2 _pointer] ; unsigned char* - only on encoding side
   [bms _bitrate-manager-state]
   [sample-count _ogg-int64]
   ))

(define-cstruct _codec-setup-info
  ([blocksizes (_list io _long 2)]
   [modes _int]
   [maps _int]
   [floors _int]
   [residues _int]
   [books _int]
   [psys _int] ; encode only
   [mode-param (_list io _vorbis-info-mode-pointer 64)]
   [map-type (_list io _int 64)]
   [map-param (_list io _vorbis-info-mapping-pointer 64)]
   [floor-type (_list io _int 64)]
   [floor-param (_list io _vorbis-info-floor-pointer 64)]
   [residue-type (_list io _int 64)]
   [residue-param (_list io _vorbis-info-residue-pointer 64)]
   [book-param (_list io _static-codebook-pointer 256)]
   [fullbooks _codebook-pointer]
   [psy-param (_list io _vorbis-info-psy-pointer 4)]
   [psy_g_param _vorbis-info-psy-global]
   [bi _bitrate-manager-info]
   [hi _highlevel-encode-setup]
   [halfrate-flag _int]))

(define-cstruct _vorbis-blockinternal
  ([pcmdelay (_ptr io (_ptr io _float))]
   [ampmax _float]
   [blocktype _int]
   [packetblob (_list io _oggpack-buffer-pointer 15)])) ; "packetblobs/2 points to the opb in the main vorbis_block"

;; codec.h
(define-cstruct _alloc-chain
  ([ptr _pointer]
   [next _alloc-chain-pointer]))

(define-cstruct _vorbis-info
  ([version _int]
   [channels _int]
   [rate _long]
   [bitrate-upper _long]
   [bitrate-nominal _long]
   [bitrate-lower _long]
   [bitrate-window _long]
   [codec-setup _pointer])) ; XXX codec_setup_info-pointer

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
   [backend-state _pointer])) ; XXX private_state-pointer

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
   [localstore _pointer] ; XXX not sure - don't think it matters - realloced every time a block is used
   [localtop _long]
   [localalloc _long]
   [totaluse _long]
   [reap _alloc-chain-pointer]
   [glue-bits _long]
   [time-bits _long]
   [floor-bits _long]
   [res-bits _long]
   [internal _vorbis-blockinternal-pointer]))

(define-cstruct _vorbis-comment
  ([user-comments (_list i _string)]
   [comment-lengths (_ptr io _int)]
   [comments _int]
   [vendor _string]))|#

;; codec.h
;; general
#|(defvorbis* (_fun _vorbis-info-pointer -> _void)
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
        -> _int))|#

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

(define-cpointer-type _vorbis-info-pointer)
(define-cpointer-type _vorbis-comment-pointer)
(define-cpointer-type _vorbis-dsp-state-pointer)
(define-cpointer-type _vorbis-block-pointer)

(defvorbis~ vorbisdec-new (_fun -> _vorbisdec-pointer))
(defvorbis~ vorbisdec-delete (_fun _vorbisdec-pointer -> _void))
(defvorbis~ vorbisdec-is-init (_fun _vorbisdec-pointer -> _bool))
(defvorbis~ vorbisdec-get-info (_fun _vorbisdec-pointer -> _vorbis-info-pointer))
(defvorbis~ vorbisdec-get-comment (_fun _vorbisdec-pointer -> _vorbis-comment-pointer))
(defvorbis~ vorbisdec-get-dsp-state (_fun _vorbisdec-pointer -> _vorbis-dsp-state-pointer))
(defvorbis~ vorbisdec-get-block (_fun _vorbisdec-pointer -> _vorbis-block-pointer))

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