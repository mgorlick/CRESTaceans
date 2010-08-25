#lang racket
(require ffi/unsafe)
(provide (all-defined-out))

(define _Allegro-Blend-Mode
  (_enum '(Allegro-Zero = 0
           Allegro-One = 1
           Allegro-Alpha = 2
           Allegro-Inverse-Alpha = 3)))

(define _Allegro-Blend-Operations
  (_enum '(Allegro-Add = 0
           Allegro-Src-Minus-Dest = 1
           Allegro-Dest-Minus-Src = 2
           Allegro-Num-Blend-Operations)))

(define _Allegro-Lock-Readwrite 0)
(define _Allegro-Lock-Readonly 1)
(define _Allegro-Lock-Writeonly 2)

(define _Allegro-Pixel-Format
  (_enum '(Allegro-Pixel-Format-Any
           Allegro-Pixel-Format-Any-No-Alpha
           Allegro-Pixel-Format-Any-With-Alpha
           Allegro-Pixel-Format-Any-15-No-Alpha
           Allegro-Pixel-Format-Any-15-With-Alpha
           Allegro-Pixel-Format-Any-16-No-Alpha
           Allegro-Pixel-Format-Any-16-With-Alpha
           Allegro-Pixel-Format-Any-24-No-Alpha
           Allegro-Pixel-Format-Any-24-With-Alpha
           Allegro-Pixel-Format-Any-32-No-Alpha
           Allegro-Pixel-Format-Any-32-With-Alpha
           Allegro-Pixel-Format-ARGB-8888
           Allegro-Pixel-Format-RGBA-8888
           Allegro-Pixel-Format-ARGB-4444
           Allegro-Pixel-Format-RGB-888
           Allegro-Pixel-Format-RGB-565
           Allegro-Pixel-Format-RGB-555
           Allegro-Pixel-Format-RGBA-5551
           Allegro-Pixel-Format-ARGB-1555
           Allegro-Pixel-Format-ABGR-8888
           Allegro-Pixel-Format-XBGR-8888
           Allegro-Pixel-Format-BGR-888
           Allegro-Pixel-Format-BGR-565
           Allegro-Pixel-Format-BGR-555
           Allegro-Pixel-Format-RGBX-8888
           Allegro-Pixel-Format-XRGB-8888
           Allegro-Pixel-Format-ABGR-F32
           Allegro-Pixel-Format-ABGR-8888-LE)))