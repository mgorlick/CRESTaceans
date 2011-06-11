#lang racket/base

(require ffi/unsafe
         "lib.rkt")
(provide (all-defined-out))

; Blending
(defallegro al-get-blender :
  (_or-null (_ptr i _int)) (_or-null (_ptr i _int))
  (_or-null (_ptr i _int))
  -> _void)
(defallegro al-get-separate-blender :
  (_or-null (_ptr i _int)) (_or-null (_ptr i _int))
  (_or-null (_ptr i _int)) (_or-null (_ptr i _int))
  (_or-null (_ptr i _int)) (_or-null (_ptr i _int))
  -> _void)
(defallegro al-set-blender : _int _int _int -> _void)
(defallegro al-set-separate-blender :
  _int _int _int _int _int _int -> _void)

; Clipping
(defallegro al-get-clipping-rectangle : 
  (_ptr i _int) (_ptr i _int) (_ptr i _int) (_ptr i _int) -> _void)
(defallegro al-set-clipping-rectangle : _int _int _int _int -> _void)

; Utility
(defallegro al-convert-mask-to-alpha : 
  _Allegro-Bitmap-pointer _Allegro-Color -> _void)