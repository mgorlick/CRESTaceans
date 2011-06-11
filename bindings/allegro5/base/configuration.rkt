#lang racket/base

(require ffi/unsafe
         "lib.rkt")
(provide (all-defined-out))

(defallegro al-create-config : -> _Allegro-Config-pointer)
(defallegro al-destroy-config : _Allegro-Config-pointer -> _void)
(defallegro al-load-config-file : _string -> _Allegro-Config-pointer)
(defallegro al-load-config-file-f : _Allegro-File-pointer -> _Allegro-Config-pointer)
(defallegro al-save-config-file : _string _Allegro-Config-pointer -> _bool)
(defallegro al-save-config-file-f :
  _Allegro-File-pointer _Allegro-Config-pointer -> _bool)
(defallegro al-add-config-section : _Allegro-Config-pointer _string -> _void)
(defallegro al-add-config-comment : _Allegro-Config-pointer _string _string -> _void)
(defallegro al-get-config-value : _Allegro-Config-pointer _string _string -> _string)
(defallegro al-set-config-value : 
  _Allegro-Config-pointer _string _string _string -> _void)
(defallegro al-get-first-config-section : 
  _Allegro-Config-pointer (_ptr io _pointer) -> _string)
(defallegro al-get-next-config-section : (_ptr io _pointer) -> _string)
(defallegro al-get-first-config-entry : 
  _Allegro-Config-pointer _string (_ptr io _pointer) -> _string)
(defallegro al-get-next-config-entry : (_ptr io _pointer) -> _string)
(defallegro al-merge-config :
  _Allegro-Config-pointer _Allegro-Config-pointer -> _Allegro-Config-pointer)
#;(defallegro al-merge-config-info : 
  _Allegro-Config-pointer _Allegro-Config-pointer -> _void)