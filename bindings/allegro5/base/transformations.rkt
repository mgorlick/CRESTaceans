#lang racket/base

(require ffi/unsafe
         "lib.rkt")
(provide (all-defined-out))

(defallegro al-copy-transform : 
  _Allegro-Transform-pointer _Allegro-Transform-pointer -> _void)
(defallegro al-use-transform : _Allegro-Transform-pointer -> _void)
(defallegro al-get-current-transform : -> _Allegro-Transform-pointer)
(defallegro al-invert-transform : _Allegro-Transform-pointer -> _void)
(defallegro al-check-inverse : _Allegro-Transform-pointer _float -> _int)
(defallegro al-identity-transform : _Allegro-Transform-pointer -> _void)
(defallegro al-build-transform : 
  _Allegro-Transform-pointer _float _float _float _float _float -> _void)
(defallegro al-translate-transform : 
  _Allegro-Transform-pointer _float _float -> _void)
(defallegro al-rotate-transform : _Allegro-Transform-pointer _float -> _void)
(defallegro al-scale-transform :
  _Allegro-Transform-pointer _float _float -> _void)
(defallegro al-transform-coordinates :
  _Allegro-Transform-pointer (_ptr i _float) (_ptr i _float) -> _void)
(defallegro al-compose-transform :
  _Allegro-Transform-pointer _Allegro-Transform-pointer -> _void)