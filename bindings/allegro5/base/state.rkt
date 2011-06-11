#lang racket/base

(require ffi/unsafe
         "lib.rkt")
(provide (all-defined-out))

(defallegro al-restore-state : _Allegro-State-pointer -> _void)
(defallegro al-store-state : _Allegro-State-pointer _int -> _void)
(defallegro al-get-errno : -> _int)
(defallegro al-set-errno : _int -> _void)