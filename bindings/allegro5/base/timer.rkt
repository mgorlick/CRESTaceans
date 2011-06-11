#lang racket/base

(require ffi/unsafe
         "lib.rkt")
(provide (all-defined-out))

(define (Allegro-Usecs-To-Secs x) (x . / . 1000000.0))
(define (Allegro-Msecs-To-Secs x) (x . / . 1000.0))
(define (Allegro-BPS-To-Secs x) (1.0 . / . x))
(define (Allegro-BPM-To-Secs x) (60.0 . / . x))

(defallegro al-create-timer : _double -> _Allegro-Timer-pointer)
(defallegro al-start-timer : _Allegro-Timer-pointer -> _void)
(defallegro al-stop-timer : _Allegro-Timer-pointer -> _void)
(defallegro al-timer-is-started : _Allegro-Timer-pointer -> _bool)
(defallegro al-destroy-timer : _Allegro-Timer-pointer -> _void)
(defallegro al-get-timer-count : _Allegro-Timer-pointer -> _int64)
(defallegro al-set-timer-count : _Allegro-Timer-pointer _int64 -> _void)
;(defallegro al-add-timer-count : _Allegro-Timer-pointer _int64 -> _void)
(defallegro al-get-timer-speed : _Allegro-Timer-pointer -> _double)
(defallegro al-set-timer-speed : _Allegro-Timer-pointer _double -> _void)
(defallegro al-get-timer-event-source : _Allegro-Timer-pointer -> _Allegro-Event-Source-pointer)