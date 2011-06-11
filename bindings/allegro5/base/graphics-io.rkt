#lang racket/base

(require ffi/unsafe
         "lib.rkt")
(provide (all-defined-out))

(defallegro al-load-bitmap : _string -> _Allegro-Bitmap-pointer)
(defallegro al-load-bitmap-f : _Allegro-File-pointer _string -> _Allegro-Bitmap-pointer)
(defallegro al-save-bitmap : _string _Allegro-Bitmap-pointer -> _bool)
(defallegro al-register-bitmap-saver : 
  _string
  (_fun _string _Allegro-Bitmap-pointer -> _bool)
  -> _bool)
(defallegro al-register-bitmap-loader :
  _string
  (_fun _string -> _Allegro-Bitmap-pointer)
  -> _bool)
(defallegro al-register-bitmap-saver-f :
  _string
  (_fun _Allegro-File-pointer _Allegro-Bitmap-pointer -> _bool)
  -> _bool)
(defallegro al-register-bitmap-loader-f :
  _string
  (_fun _Allegro-File-pointer -> _Allegro-Bitmap-pointer)
 ->  _bool)