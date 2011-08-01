#lang racket/gui

(struct video-playback (buffer bmp name w h))
(struct video-state (playback updater))

(define empty-bitmap (make-bitmap 800 600))
(define empty-video (video-playback (make-bytes (* 4 800 600))
                                    empty-bitmap "" 800 600))
(define current-video (video-state empty-video #f))

; keep track of the status of each video independent from the main playback context
(define (make-video-playback name width height)
  (define buffer (make-bytes (* 4 width height)))
  (define bmp (make-bitmap width height))
  (video-playback buffer bmp name width height))

(define (new-video-playback)
  (make-video-playback 
   (string-append "imp://127.0.0.1:8080/video" (number->string (random 65536))) 800 600))
(define (make-updater vid)
  (thread
   (λ ()
     (let loop ()
       (sleep 1/30)
       (video/randomize vid)
       (video/update vid)
       (loop)))))

(define (make-video)
  (define vid (new-video-playback))
  (video-state vid (make-updater vid)))

(define (make-videos n)
  (define videos (for/list ([i (in-range n)]) (new-video-playback)))
  (define threads (map make-updater videos))
  (map video-state videos threads))

; utility functions
(define (video/randomize v)
  (for ([i (in-range 16384)])
    (bytes-set! (video-playback-buffer v) (random (* 4 800 600)) (random 256))))

(define (video/update v)
  (send (video-playback-bmp v)
        set-argb-pixels 0 0 (video-playback-w v) (video-playback-h v) (video-playback-buffer v)))

(define video-states '())

(define (add-video)
  (define v (make-video))
  (set! video-states (cons v video-states))
  v)

;; ---------- 
;; the windows
(struct main-video-window (panel address canvas))
(struct small-video-window (panel canvas))

(define small-playback-canvas%
  (class canvas%
    [init vpb addrb]
    
    (define myvideo vpb)
    (define addressbar addrb)
    (define updater 
      (thread
       (λ ()
         (let loop ()
           (sleep 1/30)
           (send this refresh)
           (loop)))))
    
    (super-new)
    (define/override (on-event e)
      (when (send e button-down?)
        (send addressbar set-label (video-playback-name (video-state-playback myvideo)))
        (set! current-video myvideo)))))

(define (make-main-video-window parent width height)
  (define current-video-panel (new vertical-panel%
                                   [parent parent]
                                   [style '(border)]))
  (define address-bar (new message% 
                           [parent current-video-panel]
                           [min-width 500]
                           [font (make-object font% 14 'swiss)]
                           [label ""]))
  (define canvas (new canvas% 
                      [parent current-video-panel]
                      [style '(border)]
                      [min-width width]
                      [min-height height]
                      [stretchable-width #f]
                      [stretchable-height #f]
                      [paint-callback
                       (λ (canvas dc)
                         (send dc draw-bitmap 
                               (video-playback-bmp 
                                (video-state-playback current-video)) 0 0))]))
  (main-video-window current-video-panel address-bar canvas))

(define (add-small-video-playback panelobj playback)
  (new small-playback-canvas%
       [vpb playback]
       [addrb (main-video-window-address main-window)]
       [parent panelobj]
       [style '(border)]
       [min-width (/ (video-playback-w (video-state-playback playback)) 10)]
       [min-height (/ (video-playback-h (video-state-playback playback)) 10)]
       [stretchable-width #f]
       [stretchable-height #f]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap 
                (video-playback-bmp (video-state-playback playback)) 0 0))]))

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Video command center"]))

; the window onto which the current video is drawn
(define addbutton (new button%
                       [label "Add video"]
                       [parent frame]
                       [font (make-object font% 14 'swiss)]
                       [callback
                        (λ (button event)
                          (define smallvid (add-small-video-playback small-panel (add-video)))
                          (send smallvid on-event (new mouse-event% [event-type 'left-down])))]))
(define main-window (make-main-video-window frame 800 600))
(define small-panel (new horizontal-panel% 
                         [parent frame] 
                         [style '(border auto-hscroll)] 
                         [spacing 20]
                         [vert-margin 5]
                         [horiz-margin 20]
                         [min-height 80]))
; Show the frame by calling its show method
(send frame show #t)

(define refreshloop
  (thread
   (λ ()
     (let loop ()
       (sleep 1/30)
       (send (main-video-window-canvas main-window) refresh)
       (loop)))))