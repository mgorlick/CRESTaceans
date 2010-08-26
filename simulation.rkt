#lang racket

(require "../../chipmunk/chipmunk.rkt"
         (planet bzlib/thread))
(provide/contract [start-peer (-> void?)])

(define (start-peer)
  (receive/match
   [(list (? thread? source) 'spawn-request (? rational? width) (? rational? height))
    (thread-send source (list (current-thread) 'spawn-notification 
                              (thread (lambda () (init width height)))))
    (start-peer)]))

(define init
  (case-lambda
    [(width height)
     (receive/match
      [(list (? thread? sender) 'start-request (? thread? rules) (? (listof thread?) sinks))
       (thread-send sender (list (current-thread) 'start-notification))
       (init width height rules sinks)])]
    
    ; rules is ONE THREAD, while sinks is a list of threads
    [(width height rules sinks) 
     
     ; ENVIRONMENT SETUP
     
     ; required to call this before first update
     (define (newspace)
       (define space (cpSpaceNew))
       (set-cpSpace-iterations! space 5)
       (set-cpSpace-gravity! space (cpv 0.0 100.0))
       (cpSpaceResizeStaticHash space 40.0 999)
       (cpSpaceResizeActiveHash space 30.0 2999)
       space)
     
     (define height-factor ; the point on the y-axis where we start generating land 
       (/ (* 7.0 height) 10))
     
     ; make-ground-shape: cpv cpv cpSpace cpStaticBody rational rational -> void
     ; add the actual ground object to the simulation
     (define (make-ground-shape v1 v2 space staticBody height width ship)
       (let* ([real-v1 (cpv (cpVect-x v1) (+ (cpVect-y v1) 100))]
              [real-v2 (cpv (cpVect-x v2) (+ (cpVect-y v2) 100))]
              [shape (cpSpaceAddStaticShape space 
                                            (cpSegmentShapeNew staticBody 
                                                               real-v1 real-v2 100.0))])
         (cpSpaceAddCollisionHandler
          space (cpShape-collision_type ship) (cpShape-collision_type shape)
          first-callback
          second-callback
          third-callback
          fourth-callback
          #f)
         (set-cpShape-e! shape 1.0)
         (set-cpShape-u! shape 1.0)))
     
     ; make-ground: rational rational cpSpace cpStaticBody cpv -> listof-vector
     ; make data for one ground object, add to simulation, and
     ; save the point locations
     (define (make-ground width height space staticBody ship last)
       (cond
         [(> (cpVect-x last) (- width 80))
          (let* ([v2 (cpv (sub1 width) height-factor)])
            (make-ground-shape last v2 space staticBody height width ship)
            (cons (vector (sub1 width) (+ height-factor 5)) empty))]
         
         [else 
          (let* ([x (+ (random 40) 40)]
                 [y (- (random 40) 20)]
                 [end-width (+ (cpVect-x last) x)]
                 [end-height (- (cpVect-y last) y)]
                 [v2 (cpv end-width end-height)])
            (make-ground-shape last v2 space staticBody height width ship)
            (cons (vector end-width (+ end-height 5))
                  (make-ground width height space staticBody ship
                               (cpv end-width end-height)))
            )]
         ))
     
     ; SHIP SETUP
     
     (define nfuel 5000) ; starting fuel
     ; location of ship vertices with respect to center
     (define tris (vector (cpv 15.0 15.0) (cpv 0.0 -15.0) (cpv -15.0 15.0)))
     
     ; make-ship: rational rational cpSpace -> cpBody
     (define (make-ship width height space)
       (let ([body (cpBodyNew 30.0 (cpMomentForPoly 1.0 3 tris cpvzero))])
         (set-cpBody-p! body (cpv (exact->inexact (/ width 2)) 
                                  (exact->inexact (/ height 10))))
         (cpSpaceAddBody space body)
         (let ([shape (cpPolyShapeNew body 3 tris cpvzero)])
           (set-cpShape-e! shape 1.0)
           (set-cpShape-u! shape 1.0)
           (cpSpaceAddShape space shape)
           (cons body shape))
         ))
     
     ; SIMULATION START
     
     (cpInitChipmunk)
     (cpResetShapeIdCounter)
     (let* ([space (newspace)]
            [ship (make-ship width height space)]
            [staticBody (cpBodyNew +inf.0 +inf.0)]
            [state (make-hash)])
       (dict-set! state "space" space)
       (dict-set! state "ship" (car ship))
       (dict-set! state "player" nfuel)
       (dict-set! state "game" 'active)
       (dict-set! state "ground" 
                  (cons (vector 0.0 height-factor)
                        (make-ground width height space staticBody (cdr ship) 
                                     (cpv 0.0 height-factor))))
       (manage-update state width height 0.0 0.0 sinks)
       (set-simple-form! state)
       (update-loop state width height rules sinks)
       (printf "simulation freeing and shutting down~n")
       (cpSpaceFreeChildren space)
       (cpSpaceFree space))]))

(define first-callback
  (newCollisionHandler (lambda (a b c) 
                         ;(printf "callback 1~n")
                         1)))

(define second-callback
  (newCollisionHandler (lambda (a b c)
                         ;(printf "callback 2~n")
                         1)))

(define third-callback
  (newCollisionHandler2 (lambda (a b c)
                          ;(printf "callback 3~n")
                          (void))))

(define fourth-callback
  (newCollisionHandler2 (lambda (a b c)
                          ;(printf "callback 4~n")
                          (void))))

; update-loop: dict? rational? rational? listof-thread? -> void
; respond to control signal if there is one, else update with no control signal
(define (update-loop state width height rules sinks)
  (receive/match
   [(list (? thread? sender) 'permit-update! xdir ydir newstate)
    (manage-update newstate width height xdir ydir sinks)
    (update-loop newstate width height rules sinks)]
   
   [(list (? thread? sender) 'event-control (? integer? xdir) (? integer? ydir))
    ;(manage-update state width height xdir ydir sinks)
    ;(update-loop state width height rules sinks)]
   (request-update-permission state xdir ydir rules)
   (update-loop state width height rules sinks)]
   
   [(list (? thread? sender) 'shutdown)
    #f
    ]
   
   
   ))

(define (request-update-permission state xdir ydir rules)
  (thread-send rules (list (current-thread) 'permit-update? state xdir ydir)))

(define (manage-update state width height xdir ydir sinks)
  ;(sleep 0.005)
  (update state width height xdir ydir)
  (set-simple-form! state)
  (for/list ([s sinks])
    (thread-send s (list (current-thread) 'event-state state) void)))

; update: dict? rational? rational? integer? integer? -> void
(define (update state width height xdir ydir)
  (let* ([steps 3]
         [dt (/ (1.0 . / . 60.0) steps)]
         [space (dict-ref state "space")]
         [ship (dict-ref state "ship")]
         [fuel (dict-ref state "player")])
    (for [(i (in-range steps))]
      (cpSpaceStep space dt)
      )
    
    (cpBodyApplyImpulse ship (cpv 0.0 (* -100.0 ydir)) cpvzero)
    (cpBodyApplyImpulse ship (cpv (* 100.0 xdir) 0.0) cpvzero)
    (cond ; wrap the ship around if it ventures offscreen
      [(>= (xpos ship) width) 
       (set-xpos! ship (- (xpos ship) width))]
      [(< (xpos ship) 0.0)
       (set-xpos! ship (+ (xpos ship) width))])
    ))

(define (set-simple-form! state)
  (let* ([ship (dict-ref state "ship")]
         [xp (xpos ship)]
         [yp (ypos ship)]
         [xv (xvel ship)]
         [yv (yvel ship)]
         [an (angle ship)])
    (dict-set! state "simple-ship" (vector xp yp xv yv an))))

(define (number->integer n)
  (inexact->exact (round n)))

(define (angle ship-body)
  (cpBody-a ship-body))

(define (xvel ship-body)
  (cpVect-x (cpBody-v ship-body)))

(define (yvel ship-body)
  (cpVect-y (cpBody-v ship-body)))

(define (xpos ship-body)
  (cpVect-x (cpBody-p ship-body)))

(define (set-xpos! ship-body xpos)
  (set-cpVect-x! (cpBody-p ship-body) xpos))

(define (ypos ship-body)
  (cpVect-y (cpBody-p ship-body)))

(define (set-ypos! ship-body ypos)
  (set-cpVect-y! (cpBody-p ship-body) ypos))