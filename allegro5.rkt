#lang racket

(require "base/lib.rkt"
         "base/configuration.rkt"
         "base/display.rkt"
         "base/events.rkt"
         "base/fileio.rkt"
         "base/filesystem.rkt"
         "base/fixed.rkt"
         "base/graphics.rkt"
         "base/keyboard.rkt"
         "base/mouse.rkt"
         "base/opengl.rkt"
         "base/path.rkt"
         "base/state.rkt"
         "base/system.rkt"
         "base/threads.rkt"
         "base/time.rkt"
         "base/timer.rkt"
         "base/transformations.rkt"
         "addons/primitives.rkt"
         "addons/fonts.rkt"
         ffi/unsafe
         ffi/unsafe/cvector)
(provide (all-from-out "base/lib.rkt"
                       "base/configuration.rkt"
                       "base/display.rkt"
                       "base/events.rkt"
                       "base/fileio.rkt"
                       "base/filesystem.rkt"
                       "base/fixed.rkt"
                       "base/graphics.rkt"
                       "base/keyboard.rkt"
                       "base/mouse.rkt"
                       "base/opengl.rkt"
                       "base/path.rkt"
                       "base/state.rkt"
                       "base/system.rkt"
                       "base/threads.rkt"
                       "base/time.rkt"
                       "base/timer.rkt"
                       "base/transformations.rkt"
                       "addons/primitives.rkt"
                       "addons/fonts.rkt")
         (all-defined-out))

(define font (al-load-ttf-font "freefont/FreeMonoBold.ttf" 20))

(define (easy-init width height 
                   #:windowed? [windowed? #t]
                   #:fullscreen? [fullscreen? #f]
                   #:resizable? [resizable? #t]
                   #:opengl? [opengl? #t]
                   #:direct3d? [direct3d? #f]
                   #:noframe? [noframe? #f]
                   #:generate-expose-events? [generate-expose-events? #f])
  (al-install-system)
  (al-install-keyboard)
  (al-install-mouse)
  (al-init-font-addon)
  (al-init-ttf-addon)
  ;(al-install-audio ...)
  (let* ([window-mode (if (and windowed? fullscreen?) Allegro-Fullscreen-Window
                          (if fullscreen? Allegro-Fullscreen Allegro-Windowed))]
         [3dgfx-mode (if (and opengl? (not direct3d?)) Allegro-OpenGL
                         (if (and direct3d? (not opengl?)) Allegro-Direct3D-Internal 0))]
         [resize-mode (if (and windowed? resizable?) Allegro-Resizable 0)]
         [frame-mode (if noframe? Allegro-Noframe 0)]
         [event-mode (if generate-expose-events? Allegro-Generate-Expose-Events 0)]
         [mode-settings (list window-mode 3dgfx-mode resize-mode frame-mode event-mode)])
    (al-set-new-display-flags (foldl + 0 mode-settings))
    (al-create-display width height)
    ))


(define (easy-exit)
  (al-shutdown-font-addon)
  ;(al-shutdown-ttf-addon)
  (al-uninstall-keyboard)
  (al-uninstall-mouse)
  (al-uninstall-system))

(define white (al-map-rgba-f 1.0 1.0 1.0 1.0))
(define black (al-map-rgba-f 0.0 0.0 0.0 1.0))
(define green (al-map-rgba-f 0.0 1.0 0.0 1.0))
(define red (al-map-rgba-f 1.0 0.0 0.0 1.0))
(define orange (al-map-rgba-f 1.0 0.5 0.0 1.0))
(define yellow (al-map-rgba-f 1.0 1.0 0.0 1.0))
(define blue (al-map-rgba-f 0.0 0.0 1.0 1.0))
(define purple (al-map-rgba-f 1.0 0.0 1.0 1.0))