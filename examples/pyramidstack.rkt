#lang racket

(require "../chipmunk.rkt")

(cpInitChipmunk)
(cpResetShapeIdCounter)

(printf "Space setup~n")
(define space (cpSpaceNew))
(set-cpSpace-iterations! space 40)
(cpSpaceResizeStaticHash space 40.0 1000)
(cpSpaceResizeStaticHash space 40.0 1000)
(set-cpSpace-gravity! space (cpv 0.0 100.0))

(printf "Static setup~n")
(define staticBody (cpBodyNew +inf.0 +inf.0))

(define width 1024.0)
(define height 768.0)
(define hwidth (/ width 2))
(define hheight (/ height 2))

(printf "Shape 1~n")
(define left-side
  (cpSegmentShapeNew staticBody cpvzero (cpv 0.0 height) 20.0))
(set-cpShape-e! left-side 1.0)
(set-cpShape-u! left-side 1.0)
(cpSpaceAddStaticShape space left-side)

(printf "Shape 2~n")
(define right-side
  (cpSegmentShapeNew staticBody (cpv width 0.0) (cpv width height) 20.0))
(set-cpShape-e! right-side 1.0)
(set-cpShape-u! right-side 1.0)
(cpSpaceAddStaticShape space right-side)

(printf "Shape 3~n")
(define ceiling
  (cpSegmentShapeNew staticBody cpvzero (cpv width 0.0) 20.0))
(set-cpShape-e! ceiling 1.0)
(set-cpShape-u! ceiling 1.0)
(cpSpaceAddStaticShape space ceiling)

(printf "Shape 4~n")
(define floor
  (cpSegmentShapeNew staticBody (cpv 0.0 height) (cpv width height) 20.0))
(set-cpShape-e! floor 1.0)
(set-cpShape-u! floor 1.0)
(cpSpaceAddStaticShape space floor)

(printf "Bodies~n")
(define ball-radius 15.0)
(define num 5)
(define rows 14)
(define verts
  (vector (cpv 0.0 0.0)
          (cpv 0.0 30.0)
          (cpv 30.0 30.0)
          (cpv 30.0 0.0)))
(define tiles
  (for*/list ([i (in-range rows)]
              [j (in-range (add1 i))])
    (local [(define body (cpBodyNew 0.1 (cpMomentForCircle 0.1 0.0 ball-radius cpvzero)))]
      (set-cpBody-p! body (cpv (+ hwidth (- (* j 32) (* i 16)))
                               (- height (* (- rows i) 32))))
      (cpSpaceAddBody space body)
      (local [(define shape (cpCircleShapeNew body ball-radius cpvzero))]
        (set-cpShape-e! shape 0.0)
        (set-cpShape-u! shape 0.2)
        (cpSpaceAddShape space shape))
      body)))

(printf "Add a ball to make things more interesting~n")
(printf "Body~n")

(define ball-body (cpBodyNew 20.0 (cpMomentForCircle 20.0 0.0 ball-radius cpvzero)))
(set-cpBody-p! ball-body (cpv hwidth ball-radius))
(cpSpaceAddBody space ball-body)

(printf "Shape~n")
(define ball-shape (cpCircleShapeNew ball-body ball-radius cpvzero))
(set-cpShape-e! ball-shape 0.0)
(set-cpShape-u! ball-shape 1.0)
(cpSpaceAddShape space ball-shape)

(printf "Setup Done~n")
(define steps 80000)
(define rate 1/60)
(define dt (exact->inexact rate))

(require 2htdp/universe
         2htdp/image)
(define (place-image* i x y s)
  (printf "(~a,~a) @ ~a~n" x y i)
  (place-image i x y s))

(define (place-body i b s)
  (define p (cpBody-p b))
  (define x (cpVect-x p))
  (define y (cpVect-y p))
  (place-image i x y s))

(define solid-ball (circle ball-radius 'solid 'black))
(define outline-ball (circle ball-radius 'outline 'black))

(big-bang 0
          (on-draw
           (lambda (i)
             (printf "x: ~s ; y: ~s ~n" (cpVect-x (cpBody-p ball-body)) (cpVect-y (cpBody-p ball-body)))
             (place-body solid-ball
                         ball-body
                         (for/fold ([s (empty-scene width height)])
                           ([t (in-list tiles)])
                           (place-body outline-ball t s)))))
          (stop-when
           (lambda (i)
             (i . >= . steps)))
          (on-key
           (lambda (i k)
             (cpBodyApplyImpulse ball-body 
                                 (cpv 
                                  (match k
                                    ["left" -1000.0]
                                    ["right" 1000.0]
                                    [else 0.0])
                                  (match k
                                    ["up" -1000.0]
                                    ["down" 1000.0]
                                    [else 0.0]))
                                 cpvzero)
             i))
          (on-tick
           (lambda (i)
             (cpSpaceStep space dt)
             (add1 i))
           rate))

(printf "Done~n")
(cpBodyFree staticBody)
(cpSpaceFreeChildren space)
(cpSpaceFree space)
