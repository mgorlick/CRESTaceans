#lang racket

(require ffi/unsafe
         "lib.rkt")
(provide (all-defined-out))

(defallegro al-clone-bitmap : _Allegro-Bitmap-pointer -> _Allegro-Bitmap-pointer)
(defallegro al-create-bitmap : _int _int -> _Allegro-Bitmap-pointer)
(defallegro al-create-sub-bitmap : _Allegro-Bitmap-pointer _int _int _int _int -> _Allegro-Bitmap-pointer)
(defallegro al-destroy-bitmap : _Allegro-Bitmap-pointer -> _void)
(defallegro al-get-new-bitmap-flags : -> _int)
(defallegro al-get-new-bitmap-format : -> _int)
(defallegro al-set-new-bitmap-flags : _int -> _void)
(defallegro al-set-new-bitmap-format : _int -> _void)

(defallegro al-get-bitmap-flags : _Allegro-Bitmap-pointer -> _int)
(defallegro al-get-bitmap-format : _Allegro-Bitmap-pointer -> _int)
(defallegro al-get-bitmap-height : _Allegro-Bitmap-pointer -> _int)
(defallegro al-get-bitmap-width : _Allegro-Bitmap-pointer -> _int)
(defallegro al-get-pixel : _Allegro-Bitmap-pointer _int _int -> _Allegro-Color)
(defallegro al-is-bitmap-locked : _Allegro-Bitmap-pointer -> _bool)
(defallegro al-is-compatible-bitmap : _Allegro-Bitmap-pointer -> _bool)
(defallegro al-is-sub-bitmap : _Allegro-Bitmap-pointer -> _bool)