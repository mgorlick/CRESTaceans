#lang racket/base

(require ffi/unsafe
         "lib.rkt")
(provide (all-defined-out))

(defallegro al-current-time : -> _double)
(defallegro al-init-timeout : _Allegro-Timeout-pointer _double -> _void)
(defallegro al-rest : _double -> _void)