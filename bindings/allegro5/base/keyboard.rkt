#lang racket/base

(require ffi/unsafe
         "lib.rkt"
         "keycodes.rkt")
(provide (all-defined-out)
         (all-from-out "keycodes.rkt"))

(defallegro al-install-keyboard : -> _bool)
(defallegro al-is-keyboard-installed : -> _bool)
(defallegro al-uninstall-keyboard : -> _void)
;(defallegro al-get-keyboard-state : (_ptr i _Allegro-Keyboard-State) -> _void)
(defallegro al-key-down : _Allegro-Keyboard-State-pointer _int -> _bool)
(defallegro al-keycode-to-name : _int -> _string)
(defallegro al-set-keyboard-leds : _int -> _bool)
(defallegro al-get-keyboard-event-source :  -> _Allegro-Event-Source-pointer)

(define liballegro-keyboard-wrapper (ffi-lib "libracket-allegro-keyboard-wrap" "1.0.1"))
(define al-get-keyboard-state
  (get-ffi-obj "get_keyboard_state" liballegro-keyboard-wrapper
               (_fun -> (_or-null _Allegro-Keyboard-State-pointer))))
(define al-delete-keyboard-state
  (get-ffi-obj "delete_keyboard_state" liballegro-keyboard-wrapper
               (_fun _Allegro-Keyboard-State-pointer -> _void)))
