#lang racket

(require ffi/unsafe
         "lib.rkt"
         "event-types.rkt")
(provide (all-defined-out)
         (all-from-out "event-types.rkt"))

(defallegro al-create-event-queue : -> _Allegro-Event-Queue-pointer)
(defallegro al-init-user-event-source : _Allegro-Event-Queue-pointer -> _void)
(defallegro al-destroy-event-queue : _Allegro-Event-Queue-pointer -> _void)
(defallegro al-destroy-user-event-source : _Allegro-Event-Queue-pointer -> _void)
(defallegro al-drop-next-event : _Allegro-Event-Queue-pointer -> _bool)
(defallegro al-emit-user-event : 
  _Allegro-Event-Queue-pointer _Allegro-Event-pointer _pointer -> _bool) ; XXX
(defallegro al-event-queue-is-empty : _Allegro-Event-Queue-pointer -> _bool)
(defallegro al-flush-event-queue : _Allegro-Event-Queue-pointer -> _void)
(defallegro al-get-event-source-data : _Allegro-Event-Queue-pointer -> _intptr-t)
(defallegro al-get-next-event :
  _Allegro-Event-Queue-pointer _Allegro-Event-pointer -> _bool)
(defallegro al-peek-next-event :
  _Allegro-Event-Queue-pointer _Allegro-Event-pointer -> _bool)
(defallegro al-register-event-source : 
  _Allegro-Event-Queue-pointer _Allegro-Event-Source-pointer -> _void)
(defallegro al-set-event-source-data : _Allegro-Event-Queue-pointer _intptr-t -> _void)
(defallegro al-unref-user-event : _Allegro-User-Event-pointer -> _void)
(defallegro al-unregister-event-source : 
  _Allegro-Event-Queue-pointer _Allegro-Event-Source-pointer -> _void)
(defallegro al-wait-for-event : 
  _Allegro-Event-Queue-pointer (_or-null _Allegro-Event-pointer) -> _void)
(defallegro al-wait-for-event-timed :
  _Allegro-Event-Queue-pointer (_or-null _Allegro-Event-pointer) _float -> _bool)
(defallegro al-wait-for-event-until :
  _Allegro-Event-Queue-pointer (_or-null _Allegro-Event-pointer) _Allegro-Timeout-pointer -> _bool)