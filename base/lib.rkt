#lang racket

(require ffi/unsafe)
(provide (except-out (all-defined-out) allegro-func))

(define liballegro (ffi-lib "liballegro" "4.9"))

(define Allegro-Version 4)
(define Allegro-Sub-Version 9)
(define Allegro-WIP-Version 21)
(define Allegro-Release-Number 1)

(define Allegro-Version-Int (bitwise-ior
                             (arithmetic-shift Allegro-Version 24)
                             (arithmetic-shift Allegro-Sub-Version 16)
                             (arithmetic-shift Allegro-WIP-Version 8)
                             Allegro-Release-Number))

(define-syntax allegro-func
  (syntax-rules (:)
    [(_ id : x ...)
     (get-ffi-obj (regexp-replaces 'id '((#rx"-" "_"))) liballegro (_fun x ...)) ]))

(define-syntax defallegro
  (syntax-rules (:)
    [(_ id : x ...) (define id (allegro-func id : x ...))]))

(define _char _byte)
(define _uchar _ubyte)
(define _al-fixed _int32)
(define _time-t _uint32)
(define _off-t (_bytes o 64))
(define _size-t _int)
(define _intptr-t (_ptr io _int))

(define Allegro-Pi 3.14159265358979323846)

(define-cpointer-type _Allegro-Bitmap-pointer)
(define-cstruct _Allegro-Color ([r _float] [g _float] [b _float] [a _float]))
(define-cpointer-type _Allegro-Display-pointer)
(define-cstruct _Allegro-Display-Mode
  ([width _int] [height _int] [format _int] [refresh-rate _int]))

(define-cstruct _Allegro-Monitor-Info ([x1 _int] [y1 _int] [x2 _int] [y2 _int]))
(define-cpointer-type _Allegro-Event-pointer)
(define-cpointer-type _Allegro-User-Event-pointer)
(define-cpointer-type _Allegro-Event-Queue-pointer)
(define-cpointer-type _Allegro-Event-Source-pointer)
(define _Allegro-Event-Type _uint)
(define-cpointer-type _Allegro-File-pointer)
(define-cpointer-type _Allegro-File-Interface-pointer)

(define _Allegro-Seek
  (_enum '(Allegro-Seek-Set
           Allegro-Seek-Cur
           Allegro-Seek-End)))
(define-cpointer-type _Allegro-FS-Entry-pointer)
(define-cpointer-type _Allegro-FS-Interface-pointer)
(define _Allegro-File-Mode 
  (_enum '(Allegro-Filemode-Read
           Allegro-Filemode-Write
           Allegro-Filemode-Execute
           Allegro-Filemode-Hidden
           Allegro-Filemode-Isfile
           Allegro-Filemode-Isdir)))

(define-cpointer-type _Allegro-Mouse-pointer)
(define-cpointer-type _Allegro-Mouse-Cursor-pointer)
(define-cstruct _Allegro-Mouse-State
  ([x _int] [y _int] [z _int] [w _int] [more-axes (_list i _int)]
   [buttons _int] [pressure _float] [display _Allegro-Display-pointer]))
(define _Allegro-System-Mouse-Cursor
  (_enum '(Allegro-System-Mouse-Cursor-Default
           Allegro-System-Mouse-Cursor-Arrow
           Allegro-System-Mouse-Cursor-Busy
           Allegro-System-Mouse-Cursor-Question
           Allegro-System-Mouse-Cursor-Edit
           Allegro-System-Mouse-Cursor-Move
           Allegro-System-Mouse-Cursor-Resize-N
           Allegro-System-Mouse-Cursor-Resize-W
           Allegro-System-Mouse-Cursor-Resize-S
           Allegro-System-Mouse-Cursor-Resize-E
           Allegro-System-Mouse-Cursor-Resize-NW
           Allegro-System-Mouse-Cursor-Resize-SW
           Allegro-System-Mouse-Cursor-Resize-SE
           Allegro-System-Mouse-Cursor-Resize-NE
           Allegro-System-Mouse-Cursor-Progress
           Allegro-System-Mouse-Cursor-Precision
           Allegro-System-Mouse-Cursor-Link
           Allegro-System-Mouse-Cursor-Alt-Select
           Allegro-System-Mouse-Cursor-Unavailable)))

(define-cpointer-type _Allegro-Locked-Region-pointer)

(define-cpointer-type _Allegro-State-pointer)
(define-cpointer-type _Allegro-System-pointer)
(define-cpointer-type _Allegro-Config-pointer)
(define-cpointer-type _Allegro-Path-pointer)
(define-cpointer-type _Allegro-Thread-pointer)
(define-cpointer-type _Allegro-Mutex-pointer)
(define-cpointer-type _Allegro-Cond-pointer)
(define-cpointer-type _Allegro-Timeout-pointer)
(define-cpointer-type _Allegro-Timer-pointer)
(define-cpointer-type _Allegro-Transform-pointer)
(define-cpointer-type _Allegro-Ustr-pointer)

(define-cpointer-type _Allegro-Keyboard-State-pointer)

(define-cpointer-type _Allegro-OGL-EXT-List-pointer)
(define _gluint _uint)