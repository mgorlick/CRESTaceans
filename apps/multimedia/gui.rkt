#lang racket/base

(require racket/class
         racket/gui/base
         racket/contract
         "message-types.rkt"         
         "../../Motile/actor/curl.rkt"
         "../../Motile/actor/send.rkt"
         (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 2)))

(provide (rename-out [client/new-video-gui new-video-gui]
                     [client/video-gui-add-video! video-gui-add-video!])
         video-gui-clear-buffer!
         video-gui-update-buffer!)

(struct video-gui-client (gui controller@))

(define top-width 900)
(define top-height 600)

(define (make-notify-controller-callback controller@)
  (λ (m)
    (curl/send controller@ (remote/new m null #f))))

(define (client/new-video-gui controller@)
  (define the-actor-thread (current-thread))
  (parameterize ([current-eventspace (make-eventspace)])
    (video-gui-client (new-video-gui top-width top-height (make-notify-controller-callback controller@))
                      controller@)))

(define/contract (client/video-gui-add-video! client w h name)
  (video-gui-client? number? number? curl? . -> . bytes?)
  (video-playback-buffer
   (parameterize ([current-eventspace (make-eventspace)])
     (video-gui-add-video! (video-gui-client-gui client)
                           w h name (make-notify-controller-callback (video-gui-client-controller@ client))))))

;;;;;;;; ---------------------------

(define BYTES-PER-PIXEL 3)

; video-playback: used to track the metadata and state of an individual in-play, displayed video.
(struct video-playback (buffer name w h)) ; bytes? string? integer? integer?

(define (video-playback-buffersize v)
  (bytes-length (video-playback-buffer v)))

(define (make-video-playback width height name)
  (define buffer (make-bytes (* BYTES-PER-PIXEL width height)))
  (video-playback buffer name width height))

(struct video-gui (frame top-panel address-bar
                         small-video-panel) #:transparent #:mutable)

(define (video-gui-add-video! g w h name evt-cb)
  (define nv (make-video-playback w h name))
  (define address-bar (video-gui-address-bar g))
  (define small-panel (video-gui-small-video-panel g))
  (send small-panel add-video-canvas nv address-bar evt-cb)
  nv)

(define (video-gui-clear-buffer! b)
  (bytes-fill! b 0))

(define (video-gui-update-buffer! b frame)
  (bytes-copy! b 0 frame))

; ---------------------------------


(define (new-video-gui overall-width overall-height cb)
  
  (define frame (new closeable-frame% 
                     [label ""]
                     [stretchable-width #f]
                     [stretchable-height #f]
                     [min-width overall-width]
                     [min-height overall-height]
                     [style '(no-resize-border)]
                     [callback (λ () (cb (Quit/new)))]))
  
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
                            [parent the-window]
                            [alignment '(center top)]))
  
  (define host-address-button (new text-field%
                                   [label "Hostname"]
                                   [init-value "127.0.0.1"]
                                   [min-width 200]
                                   [stretchable-width #f]
                                   [parent button-panel]))
  
  (define port-address-button (new text-field%
                                   [label "Port"]
                                   [parent button-panel]
                                   [init-value "5000"]
                                   [min-width 50]
                                   [stretchable-width #f]))
  
  (define cp-button (new button%
                         [parent button-panel]
                         [label "Share this session"]
                         [callback (λ (btn ctrlevt)
                                     (cb (CopyActor/new
                                          (send host-address-button get-value) 
                                          (string->number (send port-address-button get-value)))))]))
  
  (define mv-button (new button%
                         [parent button-panel]
                         [label "Move this session"]
                         [callback (λ (btn ctrlevt)
                                     (map (λ (child)
                                            (printf "Deleting child ~a~n" child)
                                            (send frame delete-child child))
                                          (send frame get-children))
                                     (send frame show #f)
                                     (send frame enable #f)
                                     (cb (Quit/MV/new (send host-address-button get-value) 
                                                      (string->number (send port-address-button get-value))))
                                     (send frame on-close))]))
  
  
  (define top-panel (new video-top-panel%
                         [parent the-window]
                         [addressbar address-bar]
                         [alignment '(left top)]
                         [min-width (- overall-width 200)]
                         [stretchable-width #t]
                         [stretchable-height #t]))
  
  (define small-panel (new small-video-panel%
                           [parent top-panel]
                           [main-panel top-panel]
                           [hostfield host-address-button]
                           [portfield port-address-button]
                           [alignment '(left top)]
                           [style '(auto-vscroll)]
                           [min-width 200]
                           [vert-margin 10]
                           [horiz-margin 10]))
  
  (send frame show #t)
  (video-gui frame top-panel address-bar small-panel))

; ---------------------------

(define closeable-frame%
  (class frame% [init callback]
    (super-new)
    (define on-close-callback callback)
    (define/augment (on-close)
      (on-close-callback))))

(define ctrl-button%
  (class button%
    (super-new [font small-control-font])))

; ---------------------------

(define video-top-panel%
  (class horizontal-panel% [init addressbar]
    (super-new)
    
    (inherit add-child delete-child get-parent)
    (define current-canvas #f)
    (define current-canvas-sema (make-semaphore 1))
    (define address-bar addressbar)
    
    (define (clear-current-canvas)
      (when current-canvas
        (delete-child current-canvas)
        (send current-canvas show #f)
        (send current-canvas enable #f) 
        (send current-canvas stop)
        (set! current-canvas #f)))
    
    (define (replace-current-canvas v)
      (clear-current-canvas)
      (send (get-parent) min-width (video-playback-w v))
      (send (get-parent) min-height (video-playback-h v))
      (send address-bar set-label (format "~a" (curl/pretty (video-playback-name v))))
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
      (send current-canvas enable #t))
    
    (define/public (showing? v)
      (call-with-semaphore 
       current-canvas-sema
       (λ ()
         (and current-canvas 
              (send current-canvas same-video? v)))))
    
    (define/public (no-videos)
      (call-with-semaphore current-canvas-sema clear-current-canvas))
    
    (define/public (swap-focused-video v)
      (call-with-semaphore current-canvas-sema (λ () (replace-current-canvas v))))))

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
        (evt-cb (PIPOn/new pip-major-curl pip-minor-curl))
        (set! pip-major-curl #f)
        (set! pip-minor-curl #f)))
    
    (define/public (add-video-canvas v abr evt-cb)
      
      ;; holds the new SMALL video canvas.
      (define horizp 
        (new horizontal-panel% 
             [parent this]
             [alignment '(left top)]))
      
      ;; the new SMALL video canvas.
      (define cnvs
        (new small-video-canvas%
             [parent horizp]
             [cv v]
             [toppanel top-panel]
             [addressbar abr]
             [min-width (round (/ (video-playback-w v) 8))]
             [min-height (round (/ (video-playback-h v) 8))]
             [enabled #f]))
      
      ;; all control buttons go in this container.
      (define vertp 
        (new vertical-panel% 
             [parent horizp] 
             [alignment '(left top)]))
      
      (define (do-unsub btn ctrlevt)
        (evt-cb (RemoveCURL/new (video-playback-name v)))
        
        (send cnvs stop)
        (send horizp delete-child cnvs)
        (send vertp delete-child unsub)
        (send vertp delete-child cpy)
        (send vertp delete-child mv)
        (send horizp delete-child vertp)
        (send this delete-child horizp)
        
        (let ([children (send this get-children)])
          (cond
            ;; if there aren't any small canvases available notify the full-size panel as such
            [(null? children) 
             (send abr set-label "")
             (send top-panel no-videos)]
            ;; signal one of the remaining small canvases to promote itself
            [else 
             (define all-small-canvases (send (car children) get-children))
             (send (car all-small-canvases) promote-self)])))
      
      (define (do-cpy btn ctrlevt)
        (evt-cb (CopyChild/new (video-playback-name v)
                               (send host-field get-value)
                               (string->number (send port-field get-value)))))
      
      (define unsub
        (new ctrl-button%
             [parent vertp]
             [label "Unsub Stream"]
             [callback do-unsub]))
      
      (define cpy
        (new ctrl-button%
             [parent vertp]
             [label "Share Stream"]
             [callback do-cpy]))
      
      (define mv
        (new ctrl-button%
             [parent vertp]
             [label "Move Stream"]
             [callback (λ (btn ctrlevt)
                         (do-cpy cpy ctrlevt)
                         (do-unsub unsub ctrlevt))]))
      
      (define pip
        (new ctrl-button%
             [parent vertp]
             [label "Create PIP"]
             [callback (λ (btn ctrlevt)
                         (send this pip-activation-evt evt-cb (video-playback-name v)))]))
      
      (define split
        (new ctrl-button%
             [parent vertp]
             [label "Split PIP"]
             [callback (λ (btn ctrlevt)
                         (evt-cb (InitiateBehavior/new 'split (video-playback-name v))))]))
      
      (define toggle
        (new ctrl-button%
             [parent vertp]
             [label "Swap PIP"]
             [callback (λ (btn ctrlevt)
                         (evt-cb (InitiateBehavior/new 'toggle-major/minor (video-playback-name v))))]))
      
      (define encmove
        [new ctrl-button%
             [parent vertp]
             [label "Move Encoder"]
             [callback (λ (btn ctrlevt)
                         (evt-cb (FwdBackward/new (Quit/MV/new (send host-field get-value)
                                                               (string->number (send port-field get-value)))
                                                  (video-playback-name v))))]])
      
      (send cnvs enable #t)
      (send cnvs promote-self))))

; -------------------------

(define stoppable<%> 
  (interface () 
    stop))

; video-canvas% requires a video-playback struct provided as argument
; and implements an on-paint function to paint that video's current buffer with opengl
(define video-canvas%
  (class* canvas% (stoppable<%>)
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
      (new timer% 
           [notify-callback (λ () (refresh))]
           [interval 50] 
           [just-once? #f]))
    
    (define/public (stop)
      (set! myvideo #f)
      (set! buffer #f)
      (send refresher stop))
    
    (define/public (same-video? v)
      (eq? myvideo v))
    
    (define/override (on-paint)
      (with-handlers ([exn:fail? (λ _ #f)])
        (with-gl-context
         (λ ()
           (glRasterPos2d -1 1)
           (glPixelZoom 1.0 -1.0)
           (glDrawPixels w h GL_RGB GL_UNSIGNED_BYTE buffer)))
        (swap-gl-buffers))
      (resume-flush))))

; small-video-canvas%: used for the preview panes at the bottom of the screen.
(define small-video-canvas%
  (class* canvas% (stoppable<%>)
    [init cv addressbar toppanel]
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
      (new timer% 
           [notify-callback (λ () (refresh))]
           [interval 250] 
           [just-once? #f]))
    
    (define/override (on-paint)
      (with-handlers ([exn:fail? (λ _ #f)])
        (with-gl-context
         (λ ()
           (glRasterPos2d -1 1)
           (glPixelZoom 0.125 -0.125)
           (glDrawPixels actual-w actual-h GL_RGB GL_UNSIGNED_BYTE buffer)))
        (swap-gl-buffers)))
    
    (define/public (stop)
      (set! myvideo #f)
      (set! buffer #f)
      (send refresher stop))
    
    (define/override (on-event e)
      (when (and (send e button-down?)
                 (not (send top-panel showing? myvideo)))
        (send this promote-self)))
    
    (define/public (promote-self)
      (send top-panel swap-focused-video myvideo))
    
    (resume-flush)))