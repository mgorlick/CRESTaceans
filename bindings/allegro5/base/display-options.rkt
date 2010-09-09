#lang racket

(require ffi/unsafe)
(provide (all-defined-out))

(define Allegro-Windowed (arithmetic-shift 1 0))
(define Allegro-Fullscreen (arithmetic-shift 1 1))
(define Allegro-OpenGL (arithmetic-shift 1 2))
(define Allegro-Direct3D-Internal (arithmetic-shift 1 3))
(define Allegro-Resizable (arithmetic-shift 1 4))
(define Allegro-Noframe (arithmetic-shift 1 5))
(define Allegro-Generate-Expose-Events (arithmetic-shift 1 6))
(define Allegro-OpenGL-3-0 (arithmetic-shift 1 7))
(define Allegro-OpenGL-Forward-Compatible (arithmetic-shift 1 8))
(define Allegro-Fullscreen-Window (arithmetic-shift 1 9))
(define Allegro-Minimized (arithmetic-shift 1 10))

(define _Allegro-Importance
  (_enum '(Allegro-Require
           Allegro-Suggest
           Allegro-Dontcare)))

(define _Allegro-Display-Options
  (_enum '(Allegro-Red-Size
           Allegro-Green-Size
           Allegro-Blue-Size
           Allegro-Alpha-Size
           Allegro-Red-Shift
           Allegro-Green-Shift
           Allegro-Blue-Shift
           Allegro-Alpha-Shift
           Allegro-ACC-Red-Size
           Allegro-ACC-Green-Size
           Allegro-ACC-Blue-Size
           Allegro-ACC-Alpha-Size
           Allegro-Stereo
           Allegro-Aux-Buffers
           Allegro-Color-Size
           Allegro-Depth-Size
           Allegro-Stencil-Size
           Allegro-Sample-Buffers
           Allegro-Samples
           Allegro-Render-Method
           Allegro-Float-Color
           Allegro-Float-Depth
           Allegro-Single-Buffer
           Allegro-Swap-Method
           Allegro-Compatible-Display
           Allegro-Update-Display-Region
           Allegro-Vsync
           Allegro-Max-Bitmap-Size
           Allegro-Support-NPot-Bitmap
           Allegro-Can-Draw-Into-Bitmap
           Allegro-Support-Separate-Alpha
           Allegro-Display-Options-Count)))

