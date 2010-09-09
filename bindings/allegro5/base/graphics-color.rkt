#lang racket

(require "lib.rkt"
         ffi/unsafe)
(provide (all-defined-out))

(defallegro al-map-rgb : _uchar _uchar _uchar -> _Allegro-Color)
(defallegro al-map-rgb-f : _float _float _float -> _Allegro-Color)
(defallegro al-map-rgba : _uchar _uchar _uchar _uchar -> _Allegro-Color)
(defallegro al-map-rgba-f : _float _float _float _float -> _Allegro-Color)
(defallegro al-unmap-rgb : _Allegro-Color (_ptr i _uchar) (_ptr i _uchar) (_ptr i _uchar) -> _void)
(defallegro al-unmap-rgb-f : _Allegro-Color (_ptr i _float) (_ptr i _float) (_ptr i _float) -> _void)
(defallegro al-unmap-rgba : _Allegro-Color (_ptr i _uchar) (_ptr i _uchar) (_ptr i _uchar) (_ptr i _uchar) -> _void)
(defallegro al-unmap-rgba-f : _Allegro-Color (_ptr i _float) (_ptr i _float) (_ptr i _float) (_ptr i _float) -> _void)