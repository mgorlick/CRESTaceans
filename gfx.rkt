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
  (receive/match [(list (? thread? source) 'start-request (? thread? sink))
                  (thread-send source (list (current-thread) 'start-notification))
                  (io-loop main width height depth sink)])
  )

(define (io-loop main width height depth sink)
  (easy-init width height depth )
  (let ((buffer1 (image-create width height))
        (init-state #f))
    (let loop ([state init-state])
      (let ([new-state 
             (receive/match [(list (? thread? source) 'event-state w) w] 
                            [after 0 state])])
        (cond
          [(not (eq? new-state #f))
           (draw-world buffer1 new-state)
           (image-copy-screen buffer1)
           (image-clear buffer1)])
        (thread-send sink (list (current-thread) 'event-control (get-current-xdir) (get-current-ydir)))
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

(define (get-current-xdir)
  (if (keyboard-keypressed? 'A) 
      -1.0
      (if (keyboard-keypressed? 'D)
          1.0
          0.0)))

(define (get-current-ydir)
  (if (keyboard-keypressed? 'W)
      1.0
      0.0))

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
    (draw-ground buffer ground-points)
    ))

; draw-tile: buffer int int int int int int -> void
; draw a ship on the screen
(define (draw-tile buffer xp yp fuel xv yv angl) 
  (let* ([x1 (+ xp 15)]
         [y1 (+ yp 15)]
         [x2 xp]
         [y2 (- yp 15)]
         [x3 (- xp 15)]
         [y3 (+ yp 15)]
         [a (let loop ([an angl])
              (cond [(> an 359) (modulo an 359)]
                    [(< an 0) (loop (+ an 360))]
                    [else an]))]
         [x1* (x* xp yp x1 y1 angl)]
         [y1* (y* xp yp x1 y1 angl)]
         [x2* (x* xp yp x2 y2 angl)]
         [y2* (y* xp yp x2 y2 angl)]
         [x3* (x* xp yp x3 y3 angl)]
         [y3* (y* xp yp x3 y3 angl)])
    (image-triangle buffer x1* y1* x2* y2* x3* y3* (image-color 128 200 23))
    (print buffer 20 20 (format "position: [~s ~s]" xp yp))
    (print buffer 20 40 (format "velocity: [~s ~s]" xv yv))
    (print buffer 20 60 (format "fuel: ~s" fuel))
    (print buffer 20 80 (format "angle: ~s" angl))
    ))

; http://www.siggraph.org/education/materials/HyperGraph/modeling/mod_tran/2drota.htm
; x' = x cos theta - y sin theta
; translate to origin, rotate, translate back
(define (x* cx cy px py a)
  (let ((x (- px cx))
        (y (- py cy)))
    (inexact->exact (round (+ cx (- (* x (cos (/ a 360))) 
                                    (* y (sin (/ a 360))))))))
  )

; y' = y cos theta - x sin theta
; translate to origin, rotate, translate back
(define (y* cx cy px py a)
  (let ((x (- px cx))
        (y (- py cy)))
    (inexact->exact (round (+ cy (- (* y (cos (/ a 360))) 
                                    (* x (sin (/ a 360))))))))
  )

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