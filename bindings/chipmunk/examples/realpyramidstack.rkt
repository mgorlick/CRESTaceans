#lang racket/base

(require "../chipmunk.rkt"
         racket/match)

; port of chipmunk's pyramid stack demo
(cpInitChipmunk)
(cpResetShapeIdCounter)

(define height 768.0)
(define width 1024.0)
(define hwidth (/ width 2))
(define hheight (/ height 2))

(define space (cpSpaceNew))
(set-cpSpace-iterations! space 20)
(cpSpaceResizeActiveHash space 40.0 1000)
(cpSpaceResizeStaticHash space 40.0 1000)
(set-cpSpace-gravity! space (cpv 0.0 100.0))

(define staticBody (cpBodyNew +inf.0 +inf.0))

(printf "Shape 1~n")
(define left-side
  (cpSpaceAddStaticShape space (cpSegmentShapeNew staticBody cpvzero (cpv 0.0 height) 0.0)))
(set-cpShape-e! left-side 1.0)
(set-cpShape-u! left-side 1.0)

(printf "Shape 2~n")
(define right-side
  (cpSpaceAddStaticShape space (cpSegmentShapeNew staticBody (cpv width 0.0) (cpv width height) 0.0)))
(set-cpShape-e! right-side 1.0)
(set-cpShape-u! right-side 1.0)

(printf "Shape 3~n")
(define ceiling
  (cpSpaceAddStaticShape space (cpSegmentShapeNew staticBody cpvzero (cpv width 0.0) 0.0)))
(set-cpShape-e! ceiling 1.0)
(set-cpShape-u! ceiling 1.0)

(printf "Shape 4~n")
(define floor
  (cpSpaceAddStaticShape space (cpSegmentShapeNew staticBody (cpv 0.0 height) (cpv width height) 0.0)))
(set-cpShape-e! floor 1.0)
(set-cpShape-u! floor 1.0)

; boxes

(define rows 14.0)
(define radius 15.0)
(define num 4)
#;(define verts (vector (cpv 0.0 0.0)
                      (cpv 0.0 30.0)
                      (cpv 30.0 30.0)
                      (cpv 30.0 0.0)))
(define verts (vector (cpv -15.0 -15.0)
                        (cpv -15.0 15.0)
                        (cpv 15.0 15.0)
                        (cpv 15.0 -15.0)))

(define ballbody 
  (cpSpaceAddBody space (cpBodyNew 10.0 (cpMomentForCircle 10.0 0.0 radius cpvzero))))
(set-cpBody-p! ballbody (cpv hwidth (- height 20)))

(define ballshape (cpSpaceAddShape space (cpCircleShapeNew ballbody radius cpvzero)))
(set-cpShape-e! ballshape 0.0)
(set-cpShape-u! ballshape 0.8)

(define tiles
  (for*/list ([i (in-range 0.0 rows)]
              [j (in-range 0.0 (add1 i))])
    (let* ((offset (cpv (+ hwidth (- (* j 32) (* i 16))) 
                        (- height (* (- rows i) 32) 100)))
           (b (cpSpaceAddBody space 
                              (cpBodyNew 1.0
                                         ;(cpMomentForBox 1.0 30.0 30.0))))
                                         (cpMomentForPoly 1.0 num verts cpvzero))))
           (shape (cpSpaceAddShape space 
                                         ;(cpBoxShapeNew body 30.0 30.0))))
                                         (cpPolyShapeNew b num verts cpvzero))))
      (set-cpBody-p! b offset)
      (set-cpShape-e! shape 0.0)
      (set-cpShape-u! shape 0.8)
      b)))

(define rated 3)
(define rate (1/60 . / . rated))
(define dt (exact->inexact rate))

(require 2htdp/universe 2htdp/image)

(define (place-image* i x y s)
  (printf "(~a,~a) @ ~a~n" x y i)
  (place-image i x y s))

(define (place-body i b s)
  (define p (cpBody-p b))
  (define x (cpVect-x p))
  (define y (cpVect-y p))
  (place-image i x y s))

(define solid-ball (circle radius 'solid 'dodgerblue))
(define outline-square (square (* 2 radius) 'outline 'orangered))
(define solid-square (square (* 2 radius) 'solid 'orangered))


(define (deg a)
  (if (> a 359)
      (modulo (truncate a) 360)
      a))

(big-bang 0
          (on-draw
           (lambda (i)
             (place-body solid-ball
                         ballbody
                         (for/fold ([s (empty-scene width height)])
                           ([t (in-list tiles)])
                           ;(place-body solid-square t s)
                           (place-body (rotate (deg (cpBody-a t)) outline-square) t s)
                           )
                         )))
          (stop-when
           (lambda (i)
             #f))
          (on-key
           (lambda (i k)
             (cpBodyApplyImpulse ballbody 
                                 (cpv 
                                  (match k
                                    ["left" -500.0]
                                    ["right" 500.0]
                                    [else 0.0])
                                  (match k
                                    ["up" -500.0]
                                    ["down" 500.0]
                                    [else 0.0]))
                                 cpvzero)
             i))
          (on-tick
           (lambda (i)
             (for ([i (in-range rated)])
               (cpSpaceStep space dt))
             (add1 i))
           ))

(printf "Done~n")
(cpBodyFree staticBody)
(cpSpaceFreeChildren space)
(cpSpaceFree space)
