#lang racket

(require ffi/unsafe
         "lib.rkt")

(defallegro al-install-system : (_int = 0) (_pointer = #f) -> _bool)
(defallegro al-uninstall-system : -> _void)
(defallegro al-get-allegro-version : -> _uint32)
(defallegro al-get-standard-path : _int -> _Allegro-Path-pointer)
(defallegro al-set-app-name : (_ptr io _string) -> _void)
(defallegro al-set-org-name : (_ptr io _string) -> _void)
(defallegro al-get-app-name : -> _string)
(defallegro al-get-org-name : -> _string)
(defallegro al-get-system-driver : -> _Allegro-System-pointer)
(defallegro al-get-system-config : -> _Allegro-Config-pointer)