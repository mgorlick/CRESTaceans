#lang racket/base

(require ffi/unsafe
         "lib.rkt")
(provide (all-defined-out))

(defallegro al-clear-to-color : _Allegro-Color -> _void)
(defallegro al-draw-bitmap : _Allegro-Bitmap-pointer _float _float _int -> _void)
(defallegro al-draw-tinted-bitmap :
  _Allegro-Bitmap-pointer _Allegro-Color 
  _float _float _int -> _void)
(defallegro al-draw-bitmap-region :
  _Allegro-Bitmap-pointer
  _float _float
  _float _float
  _float _float
  _int -> _void)
(defallegro al-draw-tinted-bitmap-region :
  _Allegro-Bitmap-pointer _Allegro-Color
  _float _float
  _float _float
  _float _float
  _int -> _void)
(defallegro al-draw-pixel : _float _float _Allegro-Color -> _void)
(defallegro al-draw-rotated-bitmap :
  _Allegro-Bitmap-pointer
  _float _float
  _float _float
  _float _int -> _void)
(defallegro al-draw-tinted-rotated-bitmap : 
  _Allegro-Bitmap-pointer _Allegro-Color
  _float _float 
  _float _float 
  _float _int -> _void)
(defallegro al-draw-rotated-scaled-bitmap :
  _Allegro-Bitmap-pointer 
  _float _float 
  _float _float 
  _float _float
  _float _int -> _void)
(defallegro al-draw-tinted-rotated-scaled-bitmap : 
  _Allegro-Bitmap-pointer _Allegro-Color 
  _float _float
  _float _float
  _float _float 
  _float _int -> _void)
(defallegro al-draw-scaled-bitmap :
  _Allegro-Bitmap-pointer
  _float _float
  _float _float
  _float _float
  _float _float
  _int -> _void)
(defallegro al-draw-tinted-scaled-bitmap :
  _Allegro-Bitmap-pointer _Allegro-Color
  _float _float
  _float _float
  _float _float
  _float _float
  _int -> _void)
(defallegro al-get-target-bitmap : -> _Allegro-Bitmap-pointer)
(defallegro al-put-pixel : _int _int _Allegro-Color -> _void)
(defallegro al-put-blended-pixel : _int _int _Allegro-Color -> _void)
(defallegro al-set-target-bitmap : _Allegro-Bitmap-pointer  -> _void)
(defallegro al-set-target-backbuffer : _Allegro-Display-pointer -> _void)
(defallegro al-get-current-display : -> _Allegro-Display-pointer)

; Deferred drawing
(defallegro al-hold-bitmap-drawing : _bool -> _void)
(defallegro al-is-bitmap-drawing-held : -> _bool)