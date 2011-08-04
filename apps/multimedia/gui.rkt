#lang racket/gui

(require (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 2)))
(require (only-in ffi/unsafe _byte)
         (only-in ffi/unsafe/cvector make-cvector*)
         racket/contract
         racket/flonum)

(provide new-video-gui
         video-gui-add-video!
         video-playback-buffersize
         video-playback-buffer
         video-playback-lock
         video-playback-unlock)

(define BYTES-PER-PIXEL 3)

; video-playback: used to track the metadata and state of an individual in-play, displayed video.
(struct video-playback (buffer name w h sema)) ; bytes? string? integer? integer? semaphore?

(define/contract (video-playback-buffersize v)
  (video-playback? . -> . exact-nonnegative-integer?)
  (bytes-length (video-playback-buffer v)))

(define/contract (make-video-playback width height name)
  (exact-nonnegative-integer? exact-nonnegative-integer? string? . -> . video-playback?)
  (define buffer (make-bytes (* BYTES-PER-PIXEL width height)))
  (video-playback buffer name width height (make-semaphore 1)))

(define/contract (video-playback-lock v)
  (video-playback? . -> . void)
  (semaphore-wait (video-playback-sema v)))

(define/contract (video-playback-unlock v)
  (video-playback? . -> . void)
  (semaphore-post (video-playback-sema v)))

;;;;;;;; ---------------------------

; main-video-window holds the panel + message + canvas used for primary display.
;; panel%, message%, canvas%
(struct main-video-window (panel address canvas) #:transparent)

; video-gui holds the main window, plus all the small preview windows, the overall frame
; and a list of video-playback objects.
;; frame? main-video-window? (is-a/c panel%) listof video-playback
(struct video-gui (frame main-video-panel small-video-panel videos) #:transparent #:mutable)

(define/contract (video-gui-add-video! g w h name)
  (video-gui? exact-nonnegative-integer? exact-nonnegative-integer? string? . -> . video-playback?)
  
  (define nv (make-video-playback w h name))
  (set-video-gui-videos! g (cons nv (video-gui-videos g)))
  (send (video-gui-small-video-panel g) add-video-canvas nv g)
  (send (main-video-window-canvas (video-gui-main-video-panel g)) set-video nv)
  (send (main-video-window-address (video-gui-main-video-panel g)) set-label (video-playback-name nv))
  nv)

(define/contract (new-video-gui width height)
  (exact-nonnegative-integer? exact-nonnegative-integer? . -> . video-gui?)
  
  (parameterize ([current-eventspace (make-eventspace)])
    
    (define frame (new frame% [label "Video command center"]))
    
    (define current-video-panel (new vertical-panel%
                                     [parent frame]
                                     [style '(border)]))
    
    (define address-bar (new message% 
                             [label ""]
                             [parent current-video-panel]
                             [min-width 500]
                             [font (make-object font% 12 'swiss)]))
    
    (define current-video-canvas (new large-video-canvas% 
                                      [parent current-video-panel]
                                      [cv (make-video-playback width height "")] ; default blank video.
                                      [min-width width]
                                      [min-height height]))
    
    (define small-panel (new small-video-panel% 
                             [parent frame] 
                             [style '(border auto-hscroll)] 
                             [spacing 20]
                             [vert-margin 5]
                             [horiz-margin 20]
                             [min-height 80]))
    (send frame show #t)
    
    (video-gui frame (main-video-window current-video-panel address-bar current-video-canvas) small-panel '())))

(define (disable-opengl-features)
  (glDisable GL_ALPHA_TEST)
  (glDisable GL_BLEND)
  (glDisable GL_DEPTH_TEST)
  (glDisable GL_FOG)
  (glDisable GL_LIGHTING)
  (glDisable GL_LOGIC_OP)
  (glDisable GL_STENCIL_TEST)
  (glDisable GL_TEXTURE_1D)
  (glDisable GL_TEXTURE_2D)
  (glPixelTransferi GL_MAP_COLOR GL_FALSE)
  (glPixelTransferi GL_RED_SCALE 1)
  (glPixelTransferi GL_RED_BIAS 0)
  (glPixelTransferi GL_GREEN_SCALE 1)
  (glPixelTransferi GL_GREEN_BIAS 0)
  (glPixelTransferi GL_BLUE_SCALE 1)
  (glPixelTransferi GL_BLUE_BIAS 0)
  (glPixelTransferi GL_ALPHA_SCALE 1)
  (glPixelTransferi GL_ALPHA_BIAS 0))

; draw a video at normal size, flipped upside down (since Racket thinks the video is already upside down)
(define (draw-video w h cvect)
  (glRasterPos2i -1 1)
  (glPixelZoom 1.0 -1.0)
  (glDrawPixels w h GL_RGB GL_UNSIGNED_BYTE cvect))

; video-canvas% requires a video-playback struct provided as argument
; and implements an on-paint function to paint that video's current buffer with opengl
(define video-canvas%
  (class canvas%
    [init cv]
    (super-new [style '(border gl no-autoclear)]
               [stretchable-width #f]
               [stretchable-height #f])
    
    (inherit refresh is-shown? with-gl-context swap-gl-buffers)
    (field [myvideo cv])
    (field [bytescv (make-cvector* (video-playback-buffer myvideo) _byte (video-playback-buffersize myvideo))])
    (field [time-between-paint 1/30])
    
    (define (this/refresh)
      (refresh))
    (define/public (queue-refresh)
      (queue-callback this/refresh))
    
    (define/override (on-paint)
      (with-gl-context
       (λ ()
         (draw-video (video-playback-w myvideo) (video-playback-h myvideo) (video-playback-buffer myvideo))))
      (swap-gl-buffers)
      (queue-refresh))))

; large-video-canvas% holds the "main" video being displayed.
; !! FIXME !! make threadsafe
(define large-video-canvas%
  (class video-canvas% 
    (super-new)
    
    (inherit min-width min-height)
    (inherit-field myvideo bytescv)
    
    (define/public (set-video v)
      (min-width (video-playback-w v))
      (min-height (video-playback-h v))
      (set! myvideo v)
      (set! bytescv (make-cvector* (video-playback-buffer v) _byte (video-playback-buffersize v))))))

; like draw-video, but scaled to a smaller window
(define (draw-scaled-video w h scale-w scale-h cvect)
  (glRasterPos2d -1 1)
  (glPixelZoom scale-w scale-h)
  (glDrawPixels w h GL_RGB GL_UNSIGNED_BYTE cvect))

; small-video-canvas%: used for the preview panes at the bottom of the screen.
(define small-video-canvas%
  (class video-canvas%
    [init addressbar maincanvas previeww previewh]    
    (define preview-w previeww)
    (define preview-h previewh)
    
    (super-new [min-width previeww]
               [min-height previewh])
    
    (inherit queue-refresh with-gl-context swap-gl-buffers)
    (inherit-field myvideo bytescv time-between-paint)
    
    (field [address-bar addressbar])
    (field [main-canvas maincanvas])
    (define w (video-playback-w myvideo))
    (define h (video-playback-h myvideo))
    (define buffer (video-playback-buffer myvideo))
    
    (field [preview-scale-w (fl/ (->fl preview-w) (->fl w))])
    (field [preview-scale-h (fl/ (->fl preview-h) (->fl h))])
    
    (define/override (on-paint)
      (with-gl-context void (λ () (draw-scaled-video w h preview-scale-w preview-scale-h buffer)))
      (queue-refresh))
    
    (define/override (on-event e)
      (printf "event caught~n")
      (when (send e button-down?)
        (send address-bar set-label (video-playback-name myvideo))
        (send main-canvas set-video myvideo)))))

; small-video-panel: holds small-video-canvas%es as they are created.
(define small-video-panel%
  (class horizontal-panel%
    (super-new)
    
    (define/public (add-video-canvas v g)
      (new small-video-canvas%
           [cv v]
           [maincanvas (main-video-window-canvas (video-gui-main-video-panel g))]
           [addressbar (main-video-window-address (video-gui-main-video-panel g))]
           [previeww (round (/ (video-playback-w v) 8))]
           [previewh (round (/ (video-playback-h v) 8))]
           [parent this]))))