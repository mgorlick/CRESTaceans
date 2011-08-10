#lang racket/gui

(require racket/place
         racket/flonum
         (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 2)))

(provide gui-main)

(define (gui-main pls)
  (match (place-channel-get pls)
    [(list 'new-video-gui gw gh)
     (define g (parameterize ([current-eventspace (make-eventspace)]) (new-video-gui gw gh)))
     (let loop ()
       (define rq (place-channel-get pls))
       (match rq
         [(list 'add-video w h name)
          (define playback (video-gui-add-video! g w h name))
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

(struct video-gui (frame top-panel 
                         address-bar main-canvas
                         small-video-panel videos) #:transparent #:mutable)

(define/contract (video-gui-add-video! g w h name)
  (video-gui? exact-nonnegative-integer? exact-nonnegative-integer? string? . -> . video-playback?)
  
  (define nv (make-video-playback w h name))
  (set-video-gui-videos! g (cons nv (video-gui-videos g)))
  
  (define main-canvas (video-gui-main-canvas g))
  (define top-panel (video-gui-top-panel g))
  ;(define address-bar (main-video-window-address (video-gui-main-video-panel g)))
  (define small-panel (video-gui-small-video-panel g))
  (send main-canvas suspend-flush)
  (send small-panel add-video-canvas nv g)
  ;(send address-bar set-label (video-playback-name nv))
  (send main-canvas set-video nv)
  ;(send main-panel reflow-container)
  (send main-canvas resume-flush)
  nv)

; ---------------------------------

(define (new-video-gui width height)
  
  (parameterize ([current-eventspace (make-eventspace)])
    
    (define frame ; ha ha ha
      (new frame% [label "Current video"]))
    
    (define top-panel (new horizontal-panel%
                           [parent frame]))
    
    #;(define address-bar (new message% 
                               [label ""]
                               [parent current-video-panel]
                               [min-width 500]
                               [font (make-object font% 12 'swiss)]))
    
    (define small-panel (new small-video-panel%
                             [parent top-panel] 
                             ;[style '(border auto-hscroll)] 
                             [spacing 20]
                             [vert-margin 5]
                             [horiz-margin 20]
                             [min-height 80]))
    
    (define current-video-canvas (new-cvc width height top-panel #f))
    
    (send frame show #t)
    
    (video-gui frame top-panel 
               #f #|address-bar|# current-video-canvas 
               small-panel '())))

(define (new-cvc width height parent cv)
  (new large-video-canvas% 
       [parent parent]
       [cv #f]
       [min-width width]
       [min-height height]))

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
  (glRasterPos2d -1 1)
  (glPixelZoom 1.0 -1.0)
  (glDrawPixels w h GL_RGB GL_UNSIGNED_BYTE cvect))

; video-canvas% requires a video-playback struct provided as argument
; and implements an on-paint function to paint that video's current buffer with opengl
(define video-canvas%
  (class canvas%
    [init cv]
    (super-new [style '(border gl)]
               [stretchable-width #f]
               [stretchable-height #f])
    
    (inherit refresh is-shown? with-gl-context swap-gl-buffers)
    
    (field [myvideo cv]
           [time-between-paint 1/30]
           [paintlock (make-semaphore 1)])
    
    (define disabled? #f)
    
    (define (this/refresh)
      (refresh))
    (define/public (queue-refresh)
      (queue-callback this/refresh))
    
    (define/override (on-paint)
      ;(unless disabled?
      ;  (with-gl-context disable-opengl-features)
      ;  (set! disabled? #t))
      (when myvideo
        (with-gl-context
         (λ ()
           (draw-video (video-playback-w myvideo) (video-playback-h myvideo) 
                       (video-playback-buffer myvideo))
           (swap-gl-buffers))))
      (queue-refresh))))

; large-video-canvas% holds the "main" video being displayed.
; !! FIXME !! make threadsafe
(define large-video-canvas%
  (class video-canvas% 
    (super-new)
    
    (inherit get-parent suspend-flush resume-flush 
             min-width min-height get-width get-height min-client-width min-client-height
             with-gl-context swap-gl-buffers)
    (inherit-field myvideo paintlock)
    
    (define/public (set-video v)
      (suspend-flush)
      ;
      (with-gl-context
       (λ ()
         ; fixes the problem of the frame drawing halfway off the visible portion of the canvas
         (when myvideo (glViewport 0 0 (video-playback-w myvideo) (video-playback-h myvideo)))
         (set! myvideo v)
         (min-width (video-playback-w myvideo))
         (min-height (video-playback-h myvideo))
         (send (get-parent) min-width (min-width))
         (send (get-parent) min-height (min-height))
         (glViewport 0 0 (video-playback-w myvideo) (video-playback-h myvideo))))
      (resume-flush)
      (printf "resized to ~ax~a / min ~ax~a / client ~ax~a~n" 
              (get-width) (get-height) (min-width) (min-height)
              (min-client-width) (min-client-height)))))

; like draw-video, but scaled to a smaller window
(define (draw-scaled-video w h scale-w scale-h cvect)
  (glRasterPos2d -1 1)
  (glPixelZoom scale-w (- scale-h))
  (glDrawPixels w h GL_RGB GL_UNSIGNED_BYTE cvect))

; small-video-canvas%: used for the preview panes at the bottom of the screen.
(define small-video-canvas%
  (class video-canvas%
    [init addressbar maincanvas previeww previewh]   
    (super-new [min-width previeww]
               [min-height previewh])
    (inherit queue-refresh with-gl-context swap-gl-buffers)    
    (inherit-field myvideo time-between-paint)
    
    (field [address-bar addressbar])
    (field [main-canvas maincanvas])
    (field [buffer (video-playback-buffer myvideo)])
    (field [w (video-playback-w myvideo)])
    (field [h (video-playback-h myvideo)])
    (field [preview-scale-w (fl/ (->fl previeww) (->fl w))])
    (field [preview-scale-h (fl/ (->fl previewh) (->fl h))])
    
    (define disabled? #f)
    
    (define/override (on-paint)
      (unless disabled?
        (with-gl-context disable-opengl-features)
        (set! disabled? #t))
      (with-gl-context
       (λ ()
         (draw-scaled-video w h preview-scale-w preview-scale-h buffer)))
      (swap-gl-buffers)
      (queue-refresh))
    
    (define/override (on-event e)
      (printf "event caught~n")
      (when (send e button-down?)
        ;(send address-bar set-label (video-playback-name myvideo))
        (send main-canvas set-video myvideo)))))

; small-video-panel: holds small-video-canvas%es as they are created.
(define small-video-panel%
  (class vertical-panel%
    (super-new)
    
    (define/public (add-video-canvas v g)
      (new small-video-canvas%
           [cv v]
           [maincanvas (video-gui-main-canvas g)]
           [addressbar (video-gui-address-bar g)]
           [previeww (round (/ (video-playback-w v) 8))]
           [previewh (round (/ (video-playback-h v) 8))]
           [parent this]))))