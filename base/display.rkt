#lang racket

(require ffi/unsafe
         "lib.rkt"
         "display-options.rkt")
(provide (all-defined-out))

; Display creation
(defallegro al-create-display : _int _int -> _Allegro-Display-pointer)
(defallegro al-destroy-display : _Allegro-Display-pointer -> _void)
(defallegro al-get-new-display-flags : -> _int)
(defallegro al-get-new-display-refresh-rate : -> _int)
(defallegro al-get-new-window-position : (_ptr io _int) (_ptr io _int) -> _void)
(defallegro al-set-new-display-option : _int _int _int -> _void)
(defallegro al-get-new-display-option : _int (_ptr io _int) -> _int)
(defallegro al-reset-new-display-options : -> _void)
(defallegro al-set-new-display-flags : _int -> _void)
(defallegro al-set-new-display-refresh-rate : _int -> _void)
(defallegro al-set-new-window-position : _int _int -> _void)

; Display operations
(defallegro al-acknowledge-resize : _Allegro-Display-pointer -> _bool)
(defallegro al-flip-display : -> _void)
(defallegro al-get-backbuffer : _Allegro-Display-pointer -> _Allegro-Bitmap-pointer)
(defallegro al-get-frontbuffer : _Allegro-Display-pointer -> _Allegro-Bitmap-pointer)
(defallegro al-get-display-flags : _Allegro-Display-pointer -> _int)
(defallegro al-get-display-format : _Allegro-Display-pointer -> _int)
(defallegro al-get-display-height : _Allegro-Display-pointer -> _int)
(defallegro al-get-display-refresh-rate : _Allegro-Display-pointer -> _int)
(defallegro al-get-display-width : _Allegro-Display-pointer -> _int)
(defallegro al-get-window-position :
  _Allegro-Display-pointer (_ptr io _int) (_ptr io _int) -> _void)
(defallegro al-inhibit-screensaver : _bool -> _bool)
(defallegro al-resize-display : _Allegro-Display-pointer _int _int -> _bool)
(defallegro al-set-display-icon :  _Allegro-Display-pointer _Allegro-Bitmap-pointer -> _void)
(defallegro al-get-display-option : _Allegro-Display-pointer _int -> _int)
(defallegro al-set-window-position : _Allegro-Display-pointer _int _int -> _void)
(defallegro al-set-window-title : _Allegro-Display-pointer _string -> _void)
(defallegro al-toggle-display-flag : _Allegro-Display-pointer _int _bool -> _bool)
(defallegro al-update-display-region : _int _int _int _int -> _void)
(defallegro al-wait-for-vsync : -> _bool)
(defallegro al-get-display-event-source : 
  _Allegro-Display-pointer -> _Allegro-Event-Source-pointer)

; Fullscreen display modes
(defallegro al-get-display-mode :
  _int _Allegro-Display-Mode-pointer -> _Allegro-Display-Mode-pointer)
(defallegro al-get-num-display-modes : -> _int)

; Monitors
(defallegro al-get-new-display-adapter : -> _int)
(defallegro al-set-new-display-adapter : _int -> _void)
(defallegro al-get-monitor-info : _int _Allegro-Monitor-Info-pointer -> _void)
(defallegro al-get-num-video-adapters : -> _int)