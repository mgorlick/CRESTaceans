#lang racket

(require ffi/unsafe
         "lib.rkt")
(provide (all-defined-out))

(defallegro al-get-opengl-extension-list : -> _Allegro-OGL-EXT-List-pointer)
(defallegro al-get-opengl-proc-address : _string -> _pointer)
(defallegro al-get-opengl-texture : _Allegro-Bitmap-pointer -> _gluint)
(defallegro al-get-opengl-texture-size : 
  _Allegro-Bitmap-pointer (_ptr i _int) (_ptr i _int) -> _void)
(defallegro al-get-opengl-texture-position :
  _Allegro-Bitmap-pointer (_ptr i _int) (_ptr i _int) -> _void)
(defallegro al-get-opengl-fbo : _Allegro-Bitmap-pointer -> _gluint)
(defallegro al-remove-opengl-fbo : _Allegro-Bitmap-pointer -> _void)
(defallegro al-is-opengl-extension-supported : _string -> _int)
(defallegro al-get-opengl-version : -> _float)
