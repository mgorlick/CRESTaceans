#lang racket

(require "../chipmunk.rkt"
         2htdp/image
         2htdp/universe)

(define space #f)
(cpInitChipmunk)
(cpResetShapeIdCounter)
(set! space (cpSpaceNew))
(define staticBody (cpBodyNew +inf.0 +inf.0))
(set-cpSpace-iterations! space 5)
(set-cpSpace-gravity! space (cpv 0.0 500.0))
(cpSpaceResizeStaticHash space 40.0 999)
(cpSpaceResizeActiveHash space 30.0 2999)

(define num-fallers 200.0)
(define num-verts 5)

(define num-tile-rows 16.0)
(define num-tile-rows* 12.0)

(define width 1024)
(define height 768)
(define hwidth (/ width 2))
(define hheight (/ height 2))

(define tris
  (vector (cpv 15.0 15.0)
          (cpv 0.0 -10.0)
          (cpv -15.0 15.0)))

(define tiles
  (for*/list ([i (in-range 0.0 num-tile-rows)]
              [j (in-range 0.0 num-tile-rows*)])
    (let* ((stagger (* (modulo j 2.0) 40.0))
           (offset (cpv (+ stagger (* i 70.0))
                        (+ 100 (* j 70.0))))
           (body (cpBodyNew 30.0 (cpMomentForPoly 1.0 3 tris offset)))
           (shape (cpSpaceAddStaticShape space (cpPolyShapeNew #f 3 tris offset))))
      (set-cpBody-p! body offset)
      (set-cpShape-e! shape 1.0)
      (set-cpShape-u! shape 1.0)
      body)))

; vertices for pentagons
(define (make-vert i)
  (let ((angle (/ (* i pi -2.0) num-verts)))
    (cpv (* 10.0 (cos angle)) (* 10.0 (sin angle)))))

(define verts
  (vector (make-vert 0.0)
          (make-vert 1.0)
          (make-vert 2.0)
          (make-vert 3.0)
          (make-vert 4.0)))

; shapes that fall from the ceiling
(define fallers
  (for/list ([n (in-range 0.0 num-fallers)])
    (let ((body (cpBodyNew 1.0 
                           (cpMomentForPoly 1.0 num-verts verts cpvzero)))
          (x (random width)))
      (set-cpBody-p! body (cpv (exact->inexact x) 0.0))
      (cpSpaceAddBody space body)
      (let ((shape (cpSpaceAddShape space 
                                    (cpPolyShapeNew body num-verts verts cpvzero))))
        (set-cpShape-e! shape 0.0)
        (set-cpShape-u! shape 0.4)
        body))))

(define pentagon (regular-polygon 14 5 'solid 'dodgerblue))
(define tri (triangle 30 'solid 'orangered))

(define (place-body i b s)
  (define p (cpBody-p b))
  (define x (cpVect-x p))
  (define y (cpVect-y p))
  (place-image i x y s))

(define (update ticks)
  (let* ((steps 1)
         (dt (/ (/ 1.0 60.0) steps)))
    (for [(i (in-range steps))]
      (cpSpaceStep space dt)
      (for/list [(f fallers)]
        (reset-body!? f))
      )))

; reset each falling body to the top of the screen if below the bottom
(define (reset-body!? b)
  (let* ((p (cpBody-p b))
         (x (cpVect-x p))
         (y (cpVect-y p)))
    (cond
      [(or (> y height)
           (> x width)
           (< x 0))
       (let ((r (random width)))
         (set-cpBody-p! b (cpv (exact->inexact r) 0.0)))])))

; for some reason chipmunk stores degrees >= 360 instead of truncing back to 0-360
(define (deg a)
  (cond
    ((> a 359)
      (modulo (truncate a) 360))
    ((< a 0)
     (modulo (truncate a) -360))
    (else 
      a)))


(define (run)
  (big-bang 0
            (on-draw (lambda (i)
                       (for/fold
                           ([s (for/fold
                                   ([s (empty-scene width height)])
                                 ([t (in-list tiles)])
                                 (place-body tri t s))])
                         ([f (in-list fallers)])
                         (place-body (rotate (deg (/ (* 180 (cpBody-a f)) pi)) pentagon) f s))))
            (stop-when (lambda (i) #f))
            (on-tick (lambda (i) 
                       (update i)
                       (add1 i)))))

(define (teardown)
  (cpSpaceFreeChildren space)
  (cpSpaceFree space))

(run)

(provide run teardown)

;(cpSpaceFreeChildren space)
;(cpSpaceFree space)
