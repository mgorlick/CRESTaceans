#lang racket

(require ffi/unsafe
         "lib.rkt")
(provide (all-defined-out))

(define _al-proc (_fun _Allegro-Thread-pointer _pointer -> _void))
(defallegro al-create-thread : _al-proc _pointer -> _void)
(defallegro al-start-thread : _Allegro-Thread-pointer -> _void)
(defallegro al-join-thread : _Allegro-Thread-pointer (_ptr io _pointer) -> _void)
(defallegro al-set-thread-should-stop : _Allegro-Thread-pointer -> _void)
(defallegro al-get-thread-should-stop : _Allegro-Thread-pointer -> _bool)
(defallegro al-destroy-thread : _Allegro-Thread-pointer -> _void)
(defallegro al-run-detached-thread : (_fun _pointer -> _void) _pointer -> _void)
(defallegro al-create-mutex : -> _Allegro-Mutex-pointer)
(defallegro al-create-mutex-recursive : -> _Allegro-Mutex-pointer)
(defallegro al-lock-mutex : _Allegro-Mutex-pointer -> _void)
(defallegro al-unlock-mutex : _Allegro-Mutex-pointer -> _void)
(defallegro al-destroy-mutex : _Allegro-Mutex-pointer -> _void)
(defallegro al-create-cond : -> _Allegro-Cond-pointer)
(defallegro al-destroy-cond : _Allegro-Cond-pointer -> _void)
(defallegro al-wait-cond : _Allegro-Cond-pointer _Allegro-Mutex-pointer -> _void)
(defallegro al-wait-cond-timed : 
  _Allegro-Cond-pointer _Allegro-Mutex-pointer _Allegro-Timeout-pointer -> _int)
(defallegro al-broadcast-cond : _Allegro-Cond-pointer -> _void)
(defallegro al-signal-cond : _Allegro-Cond-pointer -> _void)