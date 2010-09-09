#lang racket

(require ffi/unsafe
         "lib.rkt")
(provide (all-defined-out))

(defallegro al-get-pixel-size : _int -> _int)
(defallegro al-get-pixel-format-bits : _int -> _int)
(defallegro al-lock-bitmap : _Allegro-Bitmap-pointer _int _int -> _Allegro-Locked-Region-pointer)
(defallegro al-lock-bitmap-region : 
  _Allegro-Bitmap-pointer _int _int _int _int _int _int -> _Allegro-Locked-Region-pointer)
(defallegro al-unlock-bitmap : _Allegro-Bitmap-pointer -> _void)