#lang racket

(require "../../bindings/allegro5/allegro5.rkt"
         (planet bzlib/thread))
(provide/contract [start-peer (-> void?)])

(define (start-peer)
  (receive/match
   [(list (? thread? sender) 'spawn-request
          (? integer? width) (? integer? height) (? integer? depth))
    (thread-send sender (list (current-thread) 'spawn-notification 
                              (thread (lambda () (init-instance sender width height depth)))))]
   [a (printf "gfx-start-peer else: ~s~n" a)])
  (start-peer))

(define (i->n n)
  (inexact->exact (round n)))

;; Game constants
(define msg-color white)
(define bg-color black)
(define land-outline-color white)
(define land-body-color black)
(define star-color white)
(define ship-body-color black)
(define ship-outline-color white)
(define starcount 30)
(define fire-thrust-const 1.0)
(define left-rotate-const -5.0)
(define right-rotate-const 5.0)

(struct posn (x y))

;; start a new instance
(define (init-instance main width height depth)
  (receive/match [(list (? thread? source) 'start-request (? thread? sink))
                  (thread-send source (list (current-thread) 'start-notification))
                  (io-loop main width height depth sink)]))

;; main loop
(define (io-loop main width height depth sink)
  
  (define (get-state-update old-state)
    (receive/match [(list (? thread? source) 'event-state new-state) new-state] 
                   [after 0 old-state]))
  
  (define (update-sink kbd-state)
    (thread-send sink (list (current-thread) 'event-control 
                            (get-mvmt-coef kbd-state)
                            (get-rotate-coef kbd-state))))
  
  (define (send-quit-msg)
    (thread-send main (list (current-thread) 'shutdown)))
  
  (define (end-the-game? game-state kbd-state)
    (or (al-key-down kbd-state Allegro-Key-Escape)
        (equal? 'ended (dict-ref game-state "game"))))
  
  (define (get-mvmt-coef state)
    (cond [(al-key-down state Allegro-Key-W) fire-thrust-const]
          [else 0.0]))
  
  (define (get-rotate-coef state)
    (cond [(al-key-down state Allegro-Key-A) left-rotate-const]
          [(al-key-down state Allegro-Key-D) right-rotate-const]
          [else 0.0]))
  
  (define (control-signal? kbd-state)
    (or (al-key-down kbd-state Allegro-Key-Escape)
        (al-key-down kbd-state Allegro-Key-W)
        (al-key-down kbd-state Allegro-Key-A)
        (al-key-down kbd-state Allegro-Key-D)))
  
  (define display (easy-init width height))
  
  (define init-state (do ([s #f (get-state-update s)])
                       [(dict? s) s]))
  
  (define stars (for/list ([i (in-range starcount)])
                  (posn (* width (random)) (+ 120 (* (- height 120) (random))))))
  
  (define (render-state game-state kbd-state)
    (draw-world game-state width height stars 
                (eq? (get-mvmt-coef kbd-state) fire-thrust-const)
                (eq? (get-rotate-coef kbd-state) left-rotate-const)
                (eq? (get-rotate-coef kbd-state) right-rotate-const))
    (al-flip-display)
    (al-clear-to-color bg-color)
    (draw-stars stars)
    (draw-ground (dict-ref init-state "ground") width height))
  
  (define (shutdown)
    (send-quit-msg)
    (easy-exit display))
  
  (let loop ([state init-state])
    (let* ([new-state (get-state-update state)]
           [kbd-state (al-get-keyboard-state)]
           [end-game? (end-the-game? new-state kbd-state)])
      (render-state new-state kbd-state)
      (when (control-signal? kbd-state) (update-sink kbd-state))
      (al-delete-keyboard-state kbd-state)
      (if end-game?
          (shutdown)
          (loop new-state)))))

; draw-world: buffer dict -> void
(define (draw-world state width height stars fired? left? right?)
  (let* ([vship (dict-ref state "simple-ship")]
         [xpos (vector-ref vship 0)]
         [ypos (vector-ref vship 1)]
         [xvel (vector-ref vship 2)]
         [yvel (vector-ref vship 3)]
         [angl (vector-ref vship 4)]
         [fuel (dict-ref state "player")])
    (draw-tile xpos ypos fuel xvel yvel angl width height fired? left? right?)))

; draw-tile: buffer int int int int int int -> void
; draw a ship on the screen
(define (draw-tile xp yp fuel xv yv angl width height fired? left? right?) 
  (let* ([x1 (+ xp 15)]
         [y1 (+ yp 15)]
         [x2 xp]
         [y2 (- yp 15)]
         [x3 (- xp 15)]
         [y3 y1]
         [a (modulo (exact->inexact (round angl)) 360)]
         [x1* (x* xp yp x1 y1 angl)]
         [y1* (y* xp yp x1 y1 angl)]
         [x2* (x* xp yp x2 y2 angl)]
         [y2* (y* xp yp x2 y2 angl)]
         [x3* (x* xp yp x3 y3 angl)]
         [y3* (y* xp yp x3 y3 angl)]
         [prpt (- width 400.0)])
    (al-draw-filled-triangle x1* y1* x2* y2* x3* y3* ship-body-color)
    (al-draw-triangle x1* y1* x2* y2* x3* y3* ship-outline-color 1.0)
    (al-draw-filled-circle x2* y2* 4.0 ship-body-color)
    (al-draw-circle x2* y2* 4.0 ship-outline-color 1.0)
    (when (and fired? (> fuel 0))
      (al-draw-filled-circle (midpoint x1* x3*) (midpoint y1* y3*) 6.0 ship-outline-color))
    (when (and left? (> fuel 0))
      (al-draw-filled-circle (midpoint x1* x2*) (midpoint y1* y2*) 2.0 ship-outline-color))
    (when (and right? (> fuel 0))
      (al-draw-filled-circle (midpoint x2* x3*) (midpoint y2* y3*) 2.0 ship-outline-color))
    (printlmsg 20.0 20.0 (format "POSITION  [~s, ~s]" (i->n xp) (i->n yp)))
    (printlmsg 20.0 60.0 (format "FUEL      ~s" (i->n fuel)))
    (printlmsg prpt 20.0 (format "HORIZONTAL VELOCITY  ~s →" (i->n xv)))
    (printlmsg prpt 40.0 (format "VERTICAL VELOCITY    ~s ↓" (i->n yv)))
    (printlmsg prpt 60.0 (format "ANGLE                ~s°" (i->n a)))))

(define (midpoint a b)
  (/ (+ a b) 2))

(define (deg->rad d)
  (* d ( / pi 180)))

; x' = x cos theta - y sin theta
; translate to origin, rotate, translate back
(define-syntax-rule (x* center-x center-y point-x point-y angle)
  (let ((x (- point-x center-x))
        (y (- point-y center-y)))
    (round (+ center-x 
              (- (* x (cos (deg->rad angle)))
                 (* y (sin (deg->rad angle))))))))

; y' = y cos theta - x sin theta
; translate to origin, rotate, translate back
(define-syntax-rule (y* center-x center-y point-x point-y angle)
  (let ((x (- point-x center-x))
        (y (- point-y center-y)))
    (round (+ center-y 
              (+ (* y (cos (deg->rad angle)))
                 (* x (sin (deg->rad angle))))))))

(define (printlmsg x y msg)
  (al-draw-text font msg-color x (+ 0.0 y) 0 msg))

;;;;; background

(define (draw-stars stars)
  (map (λ (p) (al-draw-filled-circle (posn-x p) (posn-y p) 2.0 star-color)) stars))

; take every adjacent pair of points on the ground, draw lines between them
; then do the same thing with the next adjacent pair 
; (i.e., on every step through draw-ground, ground-points is shortened by 1)
(define (draw-ground ground-points width height)
  (draw-ground-segment (car ground-points) (cadr ground-points) width height)
  (unless (empty? (cddr ground-points)) (draw-ground (cdr ground-points) width height)))

; draw line from v1 to v2
(define (draw-ground-segment v1 v2 width height)
  (let* ([x1 (vector-ref v1 0)]
         [y1 (vector-ref v1 1)]
         [x2 (vector-ref v2 0)]
         [y2 (vector-ref v2 1)]
         [y3 (if (>= y1 y2) y1 y2)]
         [x3 (if (>= y1 y2) x2 x1)]
         [triangle? (not (eq? y1 y2))])
    (if triangle? 
        (al-draw-filled-triangle x1 y1 x2 y2 x3 y3 land-body-color)
        (al-draw-line x1 y1 x2 y2 land-body-color 2.0))
    (al-draw-filled-rectangle x1 y3 x2 (exact->inexact height) land-body-color)
    (al-draw-line x1 y1 x2 y2 land-outline-color 1.0)))