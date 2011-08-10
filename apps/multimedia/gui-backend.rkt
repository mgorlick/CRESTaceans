#lang racket/gui

(require racket/place
         racket/flonum
         (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 2)))

(provide gui-main)

(define (gui-main pls)
  (match (place-channel-get pls)
    [(list 'new-video-gui gw gh)
     (define evtsp (make-eventspace))
     (define g (parameterize ([current-eventspace evtsp])
                 (new-video-gui gw gh)))
     (let loop ()
       (define rq (place-channel-get pls))
       (match rq
         [(list 'add-video w h name)
          (define playback 
            ;(parameterize ([current-eventspace evtsp])
            (video-gui-add-video! g w h name));)
          (place-channel-put pls (video-playback-buffer playback))
          (loop)]))]))

; ------------------------------

(define BYTES-PER-PIXEL 3)

; video-playback: used to track the metadata and state of an individual in-play, displayed video.
(struct video-playback (buffer name w h sema)) ; bytes? string? integer? integer? semaphore?

(define (video-playback-buffersize v)
  (bytes-length (video-playback-buffer v)))

(define (make-video-playback width height name)
  (define buffer (make-shared-bytes (* BYTES-PER-PIXEL width height)))
  (video-playback buffer name width height (make-semaphore 1)))

(define (video-playback-lock v)
  (semaphore-wait (video-playback-sema v)))

(define (video-playback-unlock v)
  (semaphore-post (video-playback-sema v)))

;;;;;;;; ---------------------------

(struct video-gui (frame top-panel address-bar
                         small-video-panel videos) #:transparent #:mutable)

(define/contract (video-gui-add-video! g w h name)
  (video-gui? exact-nonnegative-integer? exact-nonnegative-integer? string? . -> . video-playback?)
  
  (define nv (make-video-playback w h name))
  (set-video-gui-videos! g (cons nv (video-gui-videos g)))
  
  (define top-panel (video-gui-top-panel g))
  (define address-bar (video-gui-address-bar g))
  (define small-panel (video-gui-small-video-panel g))
  (send small-panel add-video-canvas nv address-bar)
  nv)

; ---------------------------------

(define (new-video-gui width height)   
  (define frame (new frame% 
                     [label ""]
                     [stretchable-width #f]
                     [stretchable-height #f]
                     [style '(no-resize-border)]))
  (define top-panel (new video-top-panel%
                         [parent frame]
                         [alignment '(left top)]
                         [min-width width]
                         [min-height height]
                         ;[stretchable-width #f]
                         ;[stretchable-height #f]
                         ))
  (define small-panel (new small-video-panel%
                           [parent top-panel] 
                           [alignment '(left top)]
                           [style '(vscroll)]
                           [vert-margin 10]
                           [horiz-margin 10]
                           [spacing 10]))
  (send frame show #t)
  (video-gui frame top-panel #f small-panel '()))

; ---------------------------

(define video-top-panel%
  (class horizontal-panel%
    (super-new)
    
    (inherit add-child delete-child get-parent)
    (field [current-canvas #f])
    
    (define/public (swap-focused-video v)
      (when current-canvas
        (send current-canvas show #f)
        (delete-child current-canvas))
      (send (get-parent) min-width (video-playback-w v))
      (send (get-parent) min-height (video-playback-h v))
      (set! current-canvas (new video-canvas% 
                                [parent this]
                                [cv v]
                                [vert-margin 10]
                                [horiz-margin 10]
                                [min-width (video-playback-w v)]
                                [min-height (video-playback-h v)])))))

; small-video-panel: holds small-video-canvas%es as they are created.
(define small-video-panel%
  (class vertical-panel%
    (super-new)
    
    (inherit get-parent)
    
    (define/public (add-video-canvas v abr)
      (new small-video-canvas%
           [parent this]
           [cv v]
           [toppanel (get-parent)]
           [addressbar abr]
           [min-width (round (/ (video-playback-w v) 8))]
           [min-height (round (/ (video-playback-h v) 8))])
      (sleep 0)
      (send (get-parent) swap-focused-video v))))

; -------------------------

; video-canvas% requires a video-playback struct provided as argument
; and implements an on-paint function to paint that video's current buffer with opengl
(define video-canvas%
  (class canvas%
    [init cv]
    (inherit refresh with-gl-context swap-gl-buffers)
    
    (define myvideo cv)
    (define w (video-playback-w myvideo))
    (define h (video-playback-h myvideo))
    (define buffer (video-playback-buffer myvideo))
    
    (define/override (on-paint)
      (with-gl-context
       (λ ()
         (glRasterPos2d -1 1)
         (glPixelZoom 1.0 -1.0)
         (glDrawPixels w h GL_RGB GL_UNSIGNED_BYTE buffer)))
      (swap-gl-buffers)
      (send this refresh))
    
    (super-new [style '(gl)]
               [stretchable-width #f]
               [stretchable-height #f])))

; small-video-canvas%: used for the preview panes at the bottom of the screen.
(define small-video-canvas%
  (class canvas%
    [init cv addressbar toppanel]
    (inherit refresh with-gl-context swap-gl-buffers)
    
    (define address-bar addressbar)
    (define top-panel toppanel)
    (define myvideo cv)    
    (define buffer (video-playback-buffer myvideo))
    (define actual-w (video-playback-w myvideo))
    (define actual-h (video-playback-h myvideo))
    
    (define/override (on-paint)
      (with-gl-context
       (λ ()
         (glRasterPos2d -1 1)
         (glPixelZoom 0.125 -0.125)
         (glDrawPixels actual-w actual-h GL_RGB GL_UNSIGNED_BYTE buffer)))
      (swap-gl-buffers)
      (send this refresh))
    
    (super-new [style '(gl)]
               [stretchable-width #f]
               [stretchable-height #f])
    
    (define/override (on-event e)
      (printf "event caught~n")
      (when (send e button-down?)
        ;(send address-bar set-label (video-playback-name myvideo))
        (send top-panel set-video myvideo)))))