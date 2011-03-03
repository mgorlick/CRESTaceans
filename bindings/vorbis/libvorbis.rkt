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

;; bitrate.h

(define-cstruct _bitrate-manager-state
  ([managed _int]
   [avg-reservoir _long]
   [minmax-reservoir _long]
   [avg-bitsper _long]
   [min-bitsper _long]
   [max-bitsper _long]
   [short-per-long _long]
   [avgfloat _double]
   [vb _pointer] ; XXX vorbis-block-pointer
   [choice _int]))

(define-cstruct _bitrate-manager-info
  ([avg-rate _long]
   [min-rate _long]
   [max-rate _long]
   [reservoir-bits _long]
   [reservoir-bias _double]
   [slew-damp _double]))

;; psy.h

(define PACKETBLOBS 15)

(define P_NOISECURVES 3)
(define P_BANDS 17)
(define NOISE_COMPAND_LEVELS 40)

(define-cstruct _vorbis-info-psy
  ([blockflag _int]
   [ath-adjatt _float]
   [ath-maxatt _float]
   [tone-masteratt (_list io _float P_NOISECURVES)]
   [tone-centerboost _float]
   [tone-decay _float]
   [tone-abs-limit _float]
   [toneatt (_list io _float P_BANDS)]
   [noisemaskp _int]
   [noisemaxsupp _float]
   [noisewindowlo _float]
   [noisewindowhi _float]
   [noisewindowlomin _int]
   [noisewindowhimin _int]
   [noisewindowfixed _int]
   [noiseoff (_list io (_list io _float P_NOISECURVES) P_BANDS)]
   [noisecompound (_list io _float NOISE_COMPAND_LEVELS)]
   [max-curve-db _float]
   [normal-p _int]
   [normal-start _int]
   [normal-partition _int]
   [normal-thresh _double]))

(define-cstruct _vorbis-look-psy
  ([n _int]
   [vi _vorbis-info-psy-pointer]
   [tonecurves (_ptr io (_ptr io (_ptr io _float)))]
   [noiseoffset (_ptr io (_ptr io _float))]
   [ath (_ptr io _float)]
   [octave (_ptr io _long)]
   [bark (_ptr io _long)]
   [firstoc _long]
   [shiftoc _long]
   [eighth-octave-lines _int]
   [total-octave-lines _int]
   [rate _long]
   [m-val _float]))

(define-cstruct _vorbis-info-psy-global
  ([eighth-octave-lines _int]
   [preecho-thresh (_list io _float VE_BANDS)]
   [postecho-thresh (_list io _float VE_BANDS)]
   [stretch-penalty _float]
   [preecho-minenergy _float]
   [ampmax-att-per-sec _float]
   [coupling-pkHz (_list io _int PACKETBLOBS)]
   [coupling-pointlimit (_list io (_list io _int 2) PACKETBLOBS)]
   [coupling-prepointampt (_list io _int PACKETBLOBS)]
   [coupling-postpointamp (_list io _int PACKETBLOBS)]
   [sliding-lowpass (_list io (_list io _int 2) PACKETBLOBS)]))

(define-cstruct _vorbis-look-psy-global
  ([ampmax _float]
   [channels _int]
   [gi _vorbis-info-psy-global-pointer]
   [coupling-pointlimit (_list io (_list io _int 2) P_NOISECURVES)]))

;; highlevel.h

(define-cstruct _highlevel-byblocktype
  ([tone-mask-setting _double]
   [tone-peaklimit-setting _double]
   [noise-bias-setting _double]
   [noise-compand-setting _double]))

(define-cstruct _highlevel-encode-setup
  ([set-in-stone _int]
   [setup _pointer] ; XXX
   [base-setting _double]
   [impulse-noisetune _double]
   [req _float]
   [managed _int]
   [bitrate-min _long]
   [bitrate-av _long]
   [bitrate-av-damp _double]
   [bitrate-max _long]
   [bitrate-reservoir _long]
   [bitrate-reservoir-bias _double]
   [impulse-block-p _int]
   [noise-normalize-p _int]
   [coupling-p _int]
   [stereo-point-setting _double]
   [lowpass-kHz _double]
   [lowpass-altered _int]
   [ath-floating-db _double]
   [ath-absolute-db _double]
   [amplitude-track-dbpersec _double]
   [trigger-setting _double]
   [block (_list io _highlevel-byblocktype 4)]))

;; codebook.h

(define-cstruct _static-codebook
  ([dim _long]
   [entries _long]
   [lengthlist (_ptr io _long)]
   [maptype _int]
   [q-min _long]
   [q-delta _long]
   [q-quant _int]
   [q-sequencep _int]
   [quantlist (_ptr io _long)]
   [allocedp _int]))

(define-cstruct _codebook
  ([dim _long]
   [entries _long]
   [used-entries _long]
   [c _static-codebook-pointer]
   [valuelist (_ptr io _float)]
   [codelist (_ptr io _uint32)]
   [dec-index (_ptr io _int)]
   [dec-codelengths (_ptr io _byte)]
   [dec-firsttable (_ptr io _uint32)]
   [dec-firsttablen _int]
   [dec-maxlength _int]
   [quantvals _int]
   [minval _int]
   [delta _int]))

;; mdct.h

(define _MDCT-DATA-TYPE _float)
(define _MDCT-DATA-TYPE-pointer (_ptr io _float))

(define-cstruct _mdct-lookup
  ([n _int]
   [log2n _int]
   [trig _MDCT-DATA-TYPE-pointer]
   [bitrev _int]
   [scale _MDCT-DATA-TYPE]))

;; envelope.h

(define VE_BANDS 7)
(define VE_PRE 16)
(define VE_POST 2)
(define VE_AMP (+ VE_PRE VE_POST -1))
(define VE_NEARDC 15)

(define-cstruct _envelope-filter-state
  ([ampbuf (_list io _float VE_AMP)]
   [ampptr _int]
   [nearDC (_list io _float VE_NEARDC)]
   [nearDC-acc _float]
   [nearDC-partialacc _float]
   [nearptr _int]))

(define-cstruct _envelope-band
  ([begin _int]
   [end _int]
   [window (_ptr io _float)]
   [total _float]))

(define-cstruct _envelope-lookup
  ([ch _int]
   [winlength _int]
   [searchstep _int]
   [minergy _float]
   [mdct _mdct-lookup]
   [mdct_win (_ptr io _float)]
   [band (_list io _envelope-band VE_BANDS)]
   [filter _envelope-filter-state-pointer]
   [stretch _int]
   [mark (_ptr io _int)]
   [storage _long]
   [current _long]
   [curmark _long]
   [cursor _long]))

;; smallft.h

(define-cstruct _drft-lookup
  ([n _int]
   [trigcache (_ptr io _float)]
   [splitcache (_ptr io _int)]))

;; codec_internal.h

(define-cstruct _vorbis-info-mode
  ([blockflag _int]
   [windowtype _int]
   [transformtype _int]
   [mapping _int]))

(define-cstruct _private-state
  ([ve _envelope-lookup-pointer]
   [window (_list io _int 2)]
   [transform (_list io (_ptr io _pointer) 2)] ; XXX vorbis_look_transform** transform[2];
   [fft-look (_list io _drft-lookup 2)]
   [modebits _int]
   [flr (_ptr io _pointer)] ; XXX vorbis_look_floor** flr;
   [residue (_ptr io _pointer)] ; XXX vorbis_look_residue** residue;
   [psy (_ptr io _vorbis-look-psy)]
   [psy-g-look (_ptr io _vorbis-look-psy-global)]
   [header _pointer] ; unsigned char* - only on encoding side
   [header1 _pointer] ; unsigned char* - only on encoding side
   [header2 _pointer] ; unsigned char* - only on encoding side
   [bms _bitrate-manager-state]
   [sample-count _ogg_int64]
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
   [map-param (_list io _pointer 64)] ; XXX vorbis-info-mapping-pointer
   [floor-type (_list io _int 64)]
   [floor-param (_list io _pointer 64)] ; XXX vorbis-info-floor-pointer
   [residue-type (_list io _int 64)]
   [residue-param (_list io _pointer 64)] ; XXX vorbis-info-residue-pointer
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
   [packetblob (_list io _oggpack-buffer-pointer PACKETBLOBS)])) ; "packetblobs/2 points to the opb in the main vorbis_block"

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
   [codec-setup _codec-setup-info-pointer]))

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
   [backend-state _private-state-pointer])) ; XXX private_state-pointer

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
   [vendor _string]))

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

;(define-cpointer-type _vorbis-info-pointer)
;(define-cpointer-type _vorbis-comment-pointer)
;(define-cpointer-type _vorbis-dsp-state-pointer)
;(define-cpointer-type _vorbis-block-pointer)

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