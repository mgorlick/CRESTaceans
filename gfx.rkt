#lang racket

(require "../../allegro5/allegro5.rkt"
         (planet bzlib/thread))
(provide/contract [start-peer (-> void?)])

(define (start-peer)
  (receive/match
   [(list (? thread? sender) 'spawn-request
          (? integer? width) (? integer? height) (? integer? depth))
    (thread-send sender (list (current-thread) 'spawn-notification 
                              (thread (lambda () (init-instance sender width height depth)))))
    (start-peer)]
   [a
    (printf "gfx-start-peer else: ~s~n" a)
    (start-peer)]))

(define (i->n n)
  (inexact->exact (round n)))

(define (init-instance main width height depth)
  (receive/match [(list (? thread? source) 'start-request (? thread? sink))
                  (thread-send source (list (current-thread) 'start-notification))
                  (io-loop main width height depth sink)])
  )

(define (io-loop main width height depth sink)
  (easy-init width height)
  (let ((init-state #f))
    (let loop ([state init-state])
      (let ([new-state 
             (receive/match [(list (? thread? source) 'event-state w) w] 
                            [after 0 state])])
        (cond
          [(not (eq? new-state #f))
           (draw-world new-state width height)
           (al-flip-display)
           (al-clear-to-color black)])
        (let* ((kbd-state (al-get-keyboard-state))
               (end-game? (end-the-game? kbd-state)))
          (thread-send sink (list (current-thread) 'event-control 
                                  (get-current-xdir kbd-state) 
                                  (get-current-ydir kbd-state)))
          (al-delete-keyboard-state kbd-state)
          (if end-game? ; change this to read the game state
              (begin
                (printf "gfx shutting down~n")
                (thread-send main (list (current-thread) 'shutdown))
                (easy-exit))
              (loop new-state)))))
    ))

(define (ended? state)
  (eq? 'ended (dict-ref state "game")))

(define (end-the-game? state)
  (al-key-down state Allegro-Key-Escape))

(define (get-current-xdir state)
  (if (al-key-down state Allegro-Key-A) 
      -1.0
      (if (al-key-down state Allegro-Key-D)
          1.0
          0.0)))

(define (get-current-ydir state)
  (if (al-key-down state Allegro-Key-W)
      1.0
      0.0))

; draw-world: buffer dict -> void
(define (draw-world state width height)
  (let* ([vship (dict-ref state "simple-ship")]
         [xpos (vector-ref vship 0)]
         [ypos (vector-ref vship 1)]
         [xvel (vector-ref vship 2)]
         [yvel (vector-ref vship 3)]
         [angl (vector-ref vship 4)]
         [fuel (dict-ref state "player")]
         [ground-points (dict-ref state "ground")])
    (draw-ground ground-points width height)
    (draw-tile xpos ypos fuel xvel yvel angl)
    ))

; draw-tile: buffer int int int int int int -> void
; draw a ship on the screen
(define (draw-tile xp yp fuel xv yv angl) 
  (let* ([x1 (+ xp 15)]
         [y1 (+ yp 15)]
         [x2 xp]
         [y2 (- yp 15)]
         [x3 (- xp 15)]
         [y3 (+ yp 15)]
         [a (let loop ([an angl])
              (cond [(> an 359) (modulo (exact->inexact (round an)) 359)]
                    [(< an 0) (loop (+ an 360))]
                    [else an]))]
         [x1* (x* xp yp x1 y1 angl)]
         [y1* (y* xp yp x1 y1 angl)]
         [x2* (x* xp yp x2 y2 angl)]
         [y2* (y* xp yp x2 y2 angl)]
         [x3* (x* xp yp x3 y3 angl)]
         [y3* (y* xp yp x3 y3 angl)])
    (al-draw-filled-triangle x1* y1* x2* y2* x3* y3* purple)
    (printmsg 20.0 20.0 (format "position: [~s ~s]" (i->n xp) (i->n yp)))
    (printmsg 20.0 40.0 (format "velocity: [~s ~s]" (i->n xv) (i->n yv)))
    (printmsg 20.0 60.0 (format "fuel: ~s" fuel))
    (printmsg 20.0 80.0 (format "angle: ~s" (i->n angl)))
    ))

; http://www.siggraph.org/education/materials/HyperGraph/modeling/mod_tran/2drota.htm
; x' = x cos theta - y sin theta
; translate to origin, rotate, translate back
(define-syntax-rule (x* cx cy px py a)
  (let ((x (- px cx))
        (y (- py cy)))
    (round (+ cx (- (* x (cos (/ a 360))) 
                    (* y (sin (/ a 360)))))))
  )

; y' = y cos theta - x sin theta
; translate to origin, rotate, translate back
(define-syntax-rule (y* cx cy px py a)
  (let ((x (- px cx))
        (y (- py cy)))
    (round (+ cy (- (* y (cos (/ a 360))) 
                    (* x (sin (/ a 360)))))))
  )

(define (printmsg x y msg)
  (al-draw-text font green x (+ 0.0 y) 0 msg))

; take every adjacent pair of points on the ground, draw lines between them
; then do the same thing with the next adjacent pair 
; (i.e., on every step through draw-ground, ground-points is shortened by 1)
(define (draw-ground ground-points width height)
  (cond
    [(empty? (cddr ground-points)) 
     (draw-ground-segment (car ground-points) (cadr ground-points) width height)]
    [else 
     (draw-ground-segment (car ground-points) (cadr ground-points) width height)
     (draw-ground (cdr ground-points) width height)]))

; draw line from v1 to v2
(define (draw-ground-segment v1 v2 width height)
  (let* ((x1 (vector-ref v1 0))
         (y1 (vector-ref v1 1))
         (x2 (vector-ref v2 0))
         (y2 (vector-ref v2 1))
         (y3 (if (y1 . >= . y2) y1 y2))
         (x3 (if (y1 . >= . y2) x2 x1))
         (triangle? (not (eq? y1 y2)))
         )
    ;(al-draw-line x1 y1 x2 y2 white 2.0)))
    (if triangle? 
        (al-draw-filled-triangle x1 y1 x2 y2 x3 y3 grey)
        (al-draw-line x1 y1 x2 y2 white 2.0))
    (al-draw-filled-rectangle x1 y3 x2 (exact->inexact height) grey)))