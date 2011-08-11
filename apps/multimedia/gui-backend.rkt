#lang racket/base

(require racket/place
         racket/class
         racket/gui/base
         racket/match
         (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 2)))

(provide gui-main)

(define (gui-main pls)
  (match (place-channel-get pls)
    [(list 'new-video-gui gw gh)
     (parameterize ([current-eventspace (make-eventspace)])
       (define g (new-video-gui gw gh))
       (let loop ()
         (define rq (place-channel-get pls))
         (match rq
           [(list 'add-video w h name)
            (define playback 
              (video-gui-add-video! g w h name (λ (m) (place-channel-put pls m))))
            (place-channel-put pls (video-playback-buffer playback))
            (loop)])))]))

; ------------------------------

(define (gui-message-closed-feed u)
  `#(closed-feed ,u))

(define (gui-message-closed-window)
  `#(close-window))

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

(define (video-gui-add-video! g w h name event-callback)  
  (define nv (make-video-playback w h name))
  (set-video-gui-videos! g (cons nv (video-gui-videos g)))
  
  (define top-panel (video-gui-top-panel g))
  (define address-bar (video-gui-address-bar g))
  (define small-panel (video-gui-small-video-panel g))
  (send small-panel add-video-canvas nv address-bar event-callback)
  nv)

; ---------------------------------

(define (new-video-gui width height)   
  (define frame (new frame% 
                     [label ""]
                     [stretchable-width #f]
                     [stretchable-height #f]
                     [style '(no-resize-border)]))
  (define the-window (new vertical-panel%
                          [parent frame]))
  (define address-bar (new message%
                           [parent the-window]
                           [label ""]
                           [min-width 80]
                           [auto-resize #t]
                           [font (make-object font% 12 'swiss)]))
  (define top-panel (new video-top-panel%
                         [parent frame]
                         [addressbar address-bar]
                         [alignment '(left top)]
                         [min-width width]
                         [min-height height]
                         [stretchable-width #t]
                         [stretchable-height #t]))
  (define small-panel (new small-video-panel%
                           [parent top-panel]
                           [main-panel top-panel]
                           [alignment '(left top)]
                           [style '(auto-hscroll auto-vscroll)]
                           [min-width 200]
                           [vert-margin 10]
                           [horiz-margin 10]
                           [spacing 10]))
  (send frame show #t)
  (video-gui frame top-panel address-bar small-panel '()))

; ---------------------------

(define video-top-panel%
  (class horizontal-panel% [init addressbar]
    (super-new)
    
    (inherit add-child delete-child get-parent)
    (define current-canvas #f)
    (define address-bar addressbar)
    
    (define (clear-current-canvas)
      ;; fixme: the old child can be deleted from screen,
      ;; but it's never actually garbage collected.
      (when current-canvas
        (delete-child (weak-box-value current-canvas))
        (send (weak-box-value current-canvas) show #f)
        (send (weak-box-value current-canvas) enable #f)))
    
    (define/public (showing? v)
      (and current-canvas (send (weak-box-value current-canvas) same-video? v)))
    
    (define/public (no-videos)
      (clear-current-canvas)
      (set! current-canvas #f))
    
    (define/public (swap-focused-video v)
      (clear-current-canvas)
      (send (get-parent) min-width (video-playback-w v))
      (send (get-parent) min-height (video-playback-h v))
      (send address-bar set-label (format "~a" (video-playback-name v)))
      (define cnvs
        (new video-canvas% 
             [parent this]
             [cv v]
             [vert-margin 10]
             [horiz-margin 10]
             [min-width (video-playback-w v)]
             [min-height (video-playback-h v)]
             [enabled #f]))
      (set! current-canvas (make-weak-box cnvs))
      (send (weak-box-value current-canvas) enable #t)
      (sleep 0))))

; small-video-panel: holds small-video-canvas%es as they are created.
(define small-video-panel%
  (class vertical-panel% [init main-panel]
    (super-new)
    (inherit get-parent)
    (define top-panel main-panel)
    
    (define/public (add-video-canvas v abr evt-cb)  
      (define cnvs
        (new small-video-canvas%
             [parent this]
             [cv v]
             [toppanel top-panel]
             [addressbar abr]
             [min-width (round (/ (video-playback-w v) 8))]
             [min-height (round (/ (video-playback-h v) 8))]
             [enabled #f]))
      
      (define button
        (new button%
             [parent this]
             [label "Unsubscribe"]
             [callback (λ (btn ctrlevt)
                         (evt-cb (gui-message-closed-feed (video-playback-name v)))
                         (send cnvs enable #f)
                         (send cnvs show #f)
                         (send button enable #f)
                         (send button show #f)
                         (send this delete-child cnvs)
                         (send this delete-child button)
                         (let ([children (send this get-children)])
                           (cond [(null? children) 
                                  (send abr set-label "")
                                  (send top-panel no-videos)]
                                 [else (send (car children) promote-self)])))]
             [enabled #f]))
      
      (send cnvs enable #t)
      (send button enable #t)
      (sleep 0)
      (send cnvs promote-self))))

; -------------------------

; video-canvas% requires a video-playback struct provided as argument
; and implements an on-paint function to paint that video's current buffer with opengl
(define video-canvas%
  (class canvas%
    [init cv]
    (super-new [style '(gl no-autoclear)]
               [stretchable-width #f]
               [stretchable-height #f])
    (inherit suspend-flush resume-flush)
    (suspend-flush)
    (inherit refresh is-enabled? with-gl-context swap-gl-buffers)
    
    (define myvideo cv)
    (define w (video-playback-w myvideo))
    (define h (video-playback-h myvideo))
    (define buffer (video-playback-buffer myvideo))
    
    (define refresher
      (new timer% [notify-callback (λ () (refresh))] [interval 33] [just-once? #f]))
    
    (define/public (same-video? v)
      (equal? myvideo v))
    
    (define/override (on-paint)
      (with-gl-context
       (λ ()
         (glRasterPos2d -1 1)
         (glPixelZoom 1.0 -1.0)
         (glDrawPixels w h GL_RGB GL_UNSIGNED_BYTE buffer)))
      (swap-gl-buffers)
      (unless (is-enabled?)
        (send refresher stop)))
    (resume-flush)
    
    (printf "Made a main canvas of ~ax~a~n" (send this min-width) (send this min-height))))

; small-video-canvas%: used for the preview panes at the bottom of the screen.
(define small-video-canvas%
  (class canvas% [init cv addressbar toppanel]
    (super-new [style '(gl)]
               [stretchable-width #f]
               [stretchable-height #f])
    (inherit suspend-flush resume-flush)
    (suspend-flush)
    (inherit refresh is-enabled? with-gl-context swap-gl-buffers)
    
    (define top-panel toppanel)
    (define myvideo cv)
    (define buffer (video-playback-buffer myvideo))
    (define actual-w (video-playback-w myvideo))
    (define actual-h (video-playback-h myvideo))
    
    (define refresher
      (new timer% [notify-callback (λ () (refresh))] [interval 100] [just-once? #f]))
    
    (define/override (on-paint)
      (with-gl-context
       (λ ()
         (glRasterPos2d -1 1)
         (glPixelZoom 0.125 -0.125)
         (glDrawPixels actual-w actual-h GL_RGB GL_UNSIGNED_BYTE buffer)))
      (swap-gl-buffers)
      (unless (is-enabled?)
        (send refresher stop)))
    
    (define/override (on-event e)
      (when (and (send e button-down?)
                 (not (send top-panel showing? myvideo)))
        (send this promote-self)))
    
    (define/public (promote-self)
      (send top-panel swap-focused-video myvideo))
    
    (resume-flush)))