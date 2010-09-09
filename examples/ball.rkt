#lang racket

(require "../chipmunk.rkt")

(define space (cpSpaceNew))
(set-cpSpace-gravity! space (cpv 0.0 -100.0))

(define ballbody (cpBodyNew 100.0 +inf.0)) ; 100 mass, infinite moment
(set-cpBody-p! ballbody (cpv 160.0 250.0))

(cpSpaceAddBody space ballbody)

(define ballshape (cpCircleShapeNew ballbody 20.0 cpvzero))
(set-cpShape-e! ballshape 0.5)
(set-cpShape-u! ballshape 0.8)
(set-cpShape-collision_type! ballshape 1)
(cpSpaceAddShape space ballshape)
