#lang racket/gui

(provide new-video-gui
         video-gui-add-video!
         video-playback-copy-data
         video-playback-randomize
         video-playback-buffersize
         video-playback-buffer
         video-playback-lock
         video-playback-unlock)

(struct video-playback (buffer ; bytes?
                        bmp ; bitmap%
                        name ; string?
                        w h sema)) ; integer? integer? semaphore?

(define (video-playback-buffersize v)
  (bytes-length (video-playback-buffer v)))

(define (make-video width height #:static [static #f])
  (video-playback (make-bytes (* 4 width height))
                  (make-bitmap width height)
                  (string-append "video" (number->string (current-inexact-milliseconds)))
                  width height
                  (make-semaphore 1)))

(define (video-playback-lock v)
  (semaphore-wait (video-playback-sema v)))

(define (video-playback-unlock v)
  (semaphore-post (video-playback-sema v)))

(define (video-playback-randomize v)
  (video-playback-lock v)
  (define buffer (video-playback-buffer v))
  (define size (video-playback-buffersize v))
  (for ([i (in-range 16384)])
    (bytes-set! buffer (random size) (random 256)))
  (video-playback-unlock v))

(define (video-playback-copy-data v buffer)
  (video-playback-lock v)
  (bytes-copy! (video-playback-buffer v) 0 buffer)
  (video-playback-unlock v))

;;;;;;;; ---------------------------

(struct main-video-window (panel ; panel%
                           address ; message%
                           canvas) ; canvas%
  #:transparent)

(struct video-gui (frame ; frame%
                   main-video-panel
                   small-video-panel
                   videos) ; listof video-playback
  #:transparent #:mutable)

(define (video-gui-add-video! g w h)
  (define nv (make-video w h))
  (set-video-gui-videos! g (cons nv (video-gui-videos g)))
  (send (video-gui-small-video-panel g) add-video-canvas nv) 
  nv)

(define (new-video-gui width height)
  
  (define PREVIEW-WIDTH 80)
  (define PREVIEW-HEIGHT 60)
  
  (define thisstate (video-gui #f #f #f #f))
  
  (define starting-video (make-video 800 600 #:static #t))
  (for ([i (in-range (video-playback-buffersize starting-video))])
    (bytes-set! (video-playback-buffer starting-video) i 128))
  
  (define current-video starting-video)
  
  ;; ---------- 
  ;; the window classes
  
  (define small-playback-canvas%
    (class canvas%
      [init vpb]
      
      (define myvideo vpb)
      (define updater 
        (thread
         (λ ()
           (define bmp (video-playback-bmp myvideo))
           (define w (video-playback-w myvideo))
           (define h (video-playback-h myvideo))
           (define buffer (video-playback-buffer myvideo))
           (let loop ()
             (sleep 1/60)
             (when (send this is-shown?)
               (video-playback-randomize myvideo)
               (send bmp set-argb-pixels 0 0 w h buffer)
               (send this refresh)
               (loop))))))
      (super-new)
      
      (define my-dc (send this get-dc))
      (send my-dc scale
            (/ PREVIEW-WIDTH (video-playback-w myvideo))
            (/ PREVIEW-HEIGHT (video-playback-h myvideo)))
      
      (define/override (on-paint)
        (send my-dc draw-bitmap (video-playback-bmp myvideo) 0 0))
      
      (define/override (on-event e)
        (when (send e button-down?)
          (send address-bar set-label (video-playback-name myvideo))
          (set! current-video myvideo)))))
  
  (define small-video-panel%
    (class horizontal-panel%
      
      (super-new)
      (define/public (add-video-canvas v)
        (new small-playback-canvas%
             [vpb v]
             [parent this]
             [style '(border)]
             [min-width PREVIEW-WIDTH]
             [min-height PREVIEW-HEIGHT]
             [stretchable-width #f]
             [stretchable-height #f]))))
  
  ; windows themselves
  
  (define frame (new frame% [label "Video command center"]))
  
  (define current-video-panel (new vertical-panel%
                                   [parent frame]
                                   [style '(border)]))
  (define address-bar (new message% 
                           [label ""]
                           [parent current-video-panel]
                           [min-width 500]
                           [font (make-object font% 14 'swiss)]))
  
  (define current-video-canvas (new canvas% 
                                    [parent current-video-panel]
                                    [style '(border)]
                                    [min-width width]
                                    [min-height height]
                                    [stretchable-width #t]
                                    [stretchable-height #t]
                                    [paint-callback
                                     (λ (canvas dc)
                                       (send dc draw-bitmap (video-playback-bmp current-video) 0 0))]))
    
  (define small-panel (new small-video-panel% 
                           [parent frame] 
                           [style '(border auto-hscroll)] 
                           [spacing 20]
                           [vert-margin 5]
                           [horiz-margin 20]
                           [min-height 80]))
  
  (define addbutton (new button%
                         [label "Add video"]
                         [parent frame]
                         [font (make-object font% 14 'swiss)]
                         [callback
                          (λ (button event)
                            (video-gui-add-video! thisstate 800 600))]))
  
  (define refreshloop
    (thread
     (λ ()
       (let loop ()
         (sleep 1/60)
         (when (send current-video-canvas is-shown?)
           (send current-video-canvas refresh)
           (loop))))))
  
  (define main-window (main-video-window current-video-panel address-bar current-video-canvas))
  
  (set-video-gui-frame! thisstate frame)
  (set-video-gui-main-video-panel! thisstate main-window)
  (set-video-gui-small-video-panel! thisstate small-panel)
  (set-video-gui-videos! thisstate '())
  
  (send frame show #t)
  thisstate)

(define g (new-video-gui 800 600))