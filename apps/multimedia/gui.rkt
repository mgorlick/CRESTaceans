#lang racket/base

(require racket/class
         racket/gui/base
         "message-types.rkt"
         (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 2)))

(provide (rename-out [client/new-video-gui new-video-gui]
                     [client/video-gui-add-video! video-gui-add-video!]))

(struct video-gui-client (gui actor-thread))

(define (client/new-video-gui w h)
  (define the-actor-thread (current-thread))
  (parameterize ([current-eventspace (make-eventspace)])
    (video-gui-client (new-video-gui w h (λ (m)
                                           (thread-send the-actor-thread (cons m #f))))
                      the-actor-thread)))

(define (client/video-gui-add-video! client w h name)
  (displayln "in client/video-gui-add-video!")
  (video-playback-buffer
   (parameterize ([current-eventspace (make-eventspace)])
     (video-gui-add-video! (video-gui-client-gui client)
                           w h name (λ (m)
                                      (thread-send (video-gui-client-actor-thread client) (cons m #f)))))))

;;;;;;;; ---------------------------

; glue: events -> actor-understood messages
(define gui-message-cp CP)
(define gui-message-mv Quit/MV)
(define gui-message-cp-child CP-child)
(define gui-message-pip-on PIPOn)
(define gui-message-closed-window Quit)
(define gui-message-closed-feed RemoveCURL)

(define BYTES-PER-PIXEL 3)

; video-playback: used to track the metadata and state of an individual in-play, displayed video.
(struct video-playback (buffer name w h)) ; bytes? string? integer? integer?

(define (video-playback-buffersize v)
  (bytes-length (video-playback-buffer v)))

(define (make-video-playback width height name)
  (define buffer (make-bytes (* BYTES-PER-PIXEL width height)))
  (video-playback buffer name width height))

(struct video-gui (frame top-panel address-bar
                         small-video-panel videos) #:transparent #:mutable)

(define (video-gui-add-video! g w h name evt-cb)
  (define nv (make-video-playback w h name))
  (define address-bar (video-gui-address-bar g))
  (define small-panel (video-gui-small-video-panel g))
  (send small-panel add-video-canvas nv address-bar evt-cb)
  nv)

; ---------------------------------


(define (new-video-gui width height cb)
  (define frame (new closeable-frame% 
                     [label ""]
                     [stretchable-width #f]
                     [stretchable-height #f]
                     [style '(no-resize-border)]
                     [callback (λ () (cb (gui-message-closed-window)))]))
  (define the-window (new vertical-panel%
                          [parent frame]))
  
  (define address-bar (new message%
                           [parent the-window]
                           [label ""]
                           [min-width 80]
                           [min-height 10]
                           [auto-resize #t]
                           [font (make-object font% 12 'swiss)]))
  
  (define button-panel (new horizontal-panel%
                            [parent the-window]))
  (define host (new text-field%
                    [label "Hostname"]
                    [init-value "127.0.0.1"]
                    [min-width 200]
                    [stretchable-width #f]
                    [parent button-panel]))
  (define port (new text-field%
                    [label "Port"]
                    [parent button-panel]
                    [init-value "1235"]
                    [min-width 50]
                    [stretchable-width #f]))
  (define cp-button (new button%
                         [parent button-panel]
                         [label "Share this session"]
                         [callback (λ (btn ctrlevt)
                                     (cb (gui-message-cp (send host get-value) 
                                                         (string->number (send port get-value)))))]))
  (define mv-button (new button%
                         [parent button-panel]
                         [label "Move this session"]
                         [callback (λ (btn ctrlevt)
                                     (map (λ (child) (send frame delete-child child))
                                          (send frame get-children))
                                     (send frame show #f)
                                     (send frame enable #f)
                                     (cb (gui-message-mv (send host get-value) 
                                                         (string->number (send port get-value))))
                                     (send frame on-close))]))
  
  (define top-panel (new video-top-panel%
                         [parent the-window]
                         [addressbar address-bar]
                         [alignment '(left top)]
                         [min-width width]
                         [min-height height]
                         [stretchable-width #t]
                         [stretchable-height #t]))
  (define small-panel (new small-video-panel%
                           [parent top-panel]
                           [main-panel top-panel]
                           [hostfield host]
                           [portfield port]
                           [alignment '(left top)]
                           [style '(auto-vscroll)]
                           [min-width 200]
                           [vert-margin 10]
                           [horiz-margin 10]))
  (send frame show #t)
  (video-gui frame top-panel address-bar small-panel '()))

; ---------------------------

(define closeable-frame%
  (class frame% [init callback]
    (super-new)
    (define on-close-callback callback)
    (define/augment (on-close)
      (on-close-callback))))

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
        (delete-child current-canvas)
        (send current-canvas show #f)
        (send current-canvas enable #f)
        (set! current-canvas #f)))
    
    (define/public (showing? v)
      (and current-canvas (send current-canvas same-video? v)))
    
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
      (set! current-canvas cnvs)
      (sleep 0)
      (send current-canvas enable #t))))

; small-video-panel: holds small-video-canvas%es as they are created.
(define small-video-panel%
  (class vertical-panel% [init main-panel hostfield portfield]
    (super-new)
    (inherit get-parent)
    (define top-panel main-panel)
    (define host-field hostfield)
    (define port-field portfield)
    
    (define pip-major-curl #f)
    (define pip-minor-curl #f)
    
    (define/public (pip-activation-evt evt-cb curl)
      (cond [(not pip-major-curl) (set! pip-major-curl curl)]
            [(not pip-minor-curl) (set! pip-minor-curl curl)])
      (when (and pip-major-curl pip-minor-curl)
        (evt-cb (gui-message-pip-on pip-major-curl pip-minor-curl))
        (set! pip-major-curl #f)
        (set! pip-minor-curl #f)))
    
    (define/public (add-video-canvas v abr evt-cb)
      
      (define horizp (new horizontal-panel% [parent this] [alignment '(left top)]))
      
      (define cnvs
        (new small-video-canvas%
             [parent horizp]
             [cv v]
             [toppanel top-panel]
             [addressbar abr]
             [min-width (round (/ (video-playback-w v) 8))]
             [min-height (round (/ (video-playback-h v) 8))]
             [enabled #f]))
      
      (define vertp (new vertical-panel% [parent horizp] [alignment '(left top)]))
      
      (define (do-unsub btn ctrlevt)
        (evt-cb (gui-message-closed-feed (video-playback-name v)))
        (send cnvs show #f)
        (send cnvs enable #f)
        (send unsub show #f)
        (send unsub enable #f)
        (send cpy show #f)
        (send cpy enable #f)
        (send mv show #f)
        (send mv enable #f)
        (send horizp delete-child cnvs)
        (send vertp delete-child unsub)
        (send vertp delete-child cpy)
        (send vertp delete-child mv)
        (send horizp delete-child vertp)
        (send this delete-child horizp)
        (let ([children (send this get-children)])
          (cond [(null? children) 
                 (send abr set-label "")
                 (send top-panel no-videos)]
                [else (send (car (send (car children) get-children)) promote-self)])))
      
      (define (do-cpy btn ctrlevt)
        (evt-cb (gui-message-cp-child (video-playback-name v)
                                      (send host-field get-value)
                                      (string->number (send port-field get-value)))))
      
      (define unsub
        (new button%
             [parent vertp]
             [label "Unsubscribe"]
             [callback do-unsub]))
      
      (define cpy
        (new button%
             [parent vertp]
             [label "Share"]
             [callback do-cpy]))
      
      (define mv
        (new button%
             [parent vertp]
             [label "Move"]
             [callback (λ (btn ctrlevt)
                         (do-cpy cpy ctrlevt)
                         (do-unsub unsub ctrlevt))]))
      
      (define pip
        (new button%
             [parent vertp]
             [label "PIP"]
             [callback (λ (btn ctrlevt)
                         (send this pip-activation-evt evt-cb (video-playback-name v)))]))
      
      (sleep 0)
      (send cnvs enable #t)
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
    (resume-flush)))

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
    (define actual-w (video-playback-w cv))
    (define actual-h (video-playback-h cv))
    
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