#lang racket

(require (prefix-in keyboard- "../../allegro/keyboard.ss")
         (prefix-in image- "../../allegro/image.ss")
         "../../allegro/util.ss"
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

(define (init-instance main width height depth)
  (receive/match [(list (? thread? source) 'start-request)
                  (thread-send source (list (current-thread) 'start-notification))])
  (easy-init width height depth)
  (let ((buffer (image-create screen-x screen-y))
        (init-state #f))
    (let loop ([state init-state])
      (let ([new-state 
             (receive/match [(list (? thread? source) 'event-state w) w] 
                            [after 0 state])])
        (cond
          [(not (eq? new-state #f))
           (draw-world buffer new-state)
           (image-copy-screen buffer)
           (image-clear buffer)])
        (if (keyboard-keypressed? 'ESC) ; change this to read the game state
            (begin 
              (printf "gfx shutting down~n")
              (thread-send main (list (current-thread) 'shutdown))
              (easy-exit))
            (loop new-state))))
    ))

(define (ended? state)
  (eq? 'ended (dict-ref state "game")))

(define (number->integer n)
  (inexact->exact (round n)))

; draw-world: buffer dict -> void
(define (draw-world buffer state)
  (let* ([vship (dict-ref state "simple-ship")]
         [xpos (number->integer (vector-ref vship 0))]
         [ypos (number->integer (vector-ref vship 1))]
         [xvel (number->integer (vector-ref vship 2))]
         [yvel (number->integer (vector-ref vship 3))]
         [angl (number->integer (vector-ref vship 4))]
         [fuel (number->integer (dict-ref state "player"))]
         [ground-points (dict-ref state "ground")])
    (draw-tile buffer xpos ypos fuel xvel yvel angl)
    (draw-ground buffer ground-points)))

; draw-tile: buffer int int int int int int -> void
; draw a ship on the screen
(define (draw-tile buffer xp yp fuel xv yv angl) 
  (let ([x1 (+ xp 15)]
        [y1 (+ yp 15)]
        [x2 xp]
        [y2 (- yp 15)]
        [x3 (- xp 15)]
        [y3 (+ yp 15)])
    (image-triangle buffer x1 y1 x2 y2 x3 y3 (image-color 128 200 23))
    (print buffer 20 20 (format "position: [~s ~s]" xp yp))
    (print buffer 20 40 (format "velocity: [~s ~s]" xv yv))
    (print buffer 20 60 (format "fuel: ~s" fuel))
    (print buffer 20 80 (format "angle: ~s" angl))
    ))

(define (print buffer x y msg)
  (image-print buffer x y (image-color 255 255 255) (image-color 0 0 0) msg))

; take every adjacent pair of points on the ground, draw lines between them
; then do the same thing with the next adjacent pair 
; (i.e., on every step through draw-ground, ground-points is shortened by 1)
(define (draw-ground buffer ground-points)
  (cond
    [(empty? (cddr ground-points)) (draw-ground-segment buffer 
                                                        (car ground-points) 
                                                        (cadr ground-points))]
    [else (draw-ground-segment buffer (car ground-points) (cadr ground-points))
          (draw-ground buffer (cdr ground-points))]))

; draw line from v1 to v2
(define (draw-ground-segment buffer v1 v2)
  (image-line buffer
              (number->integer (vector-ref v1 0)) (number->integer (vector-ref v1 1))
              (number->integer (vector-ref v2 0)) (number->integer (vector-ref v2 1))
              (image-color 255 255 255)))