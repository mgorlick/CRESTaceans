#lang racket

(require ffi/unsafe
         "lib.rkt"
         "keycodes.rkt")
(provide (all-defined-out))

(defallegro al-install-keyboard : -> _bool)
(defallegro al-is-keyboard-installed : -> _bool)
(defallegro al-uninstall-keyboard : -> _void)
(defallegro al-get-keyboard-state : _Allegro-Keyboard-State-pointer -> _void)
(defallegro al-key-down : _Allegro-Keyboard-State-pointer _int -> _void)
(defallegro al-keycode-to-name : _int -> _string)
(defallegro al-set-keyboard-leds : _int -> _bool)
(defallegro al-get-keyboard-event-source :  -> _Allegro-Event-Source-pointer)