#lang racket

(require ffi/unsafe
         "lib.rkt")
(provide (all-defined-out))

(define Allegro-Event-Key-Down 10)
(define Allegro-Event-Key-Repeat 11)
(define Allegro-Event-Key-Up 12)

(define-cstruct _Allegro-Keyboard-Event
  ([type _Allegro-Event-Type]
   [srctype _Allegro-Keyboard-pointer]
   [timestamp _double]
   [keycode _int]
   [unichar _uint]
   [modifiers _uint]))