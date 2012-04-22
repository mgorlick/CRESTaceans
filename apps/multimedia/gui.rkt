#lang racket/base

(require racket/class
         racket/gui/base
         racket/contract
         ffi/vector
         "message-types.rkt"
         "config.rkt"
         "../../Motile/actor/curl.rkt"
         "../../Motile/actor/send.rkt"
         (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 2)))

(provide (rename-out [client/new-video-gui new-video-gui]
                     [client/video-gui-add-video! video-gui-add-video!])
         video-gui-clear-buffer!
         video-gui-update-buffer!)

; hash table: string (dns name) => list of string (port)
(define dns=>ports
  (let* ([known-island-addresses (hash-keys PUBLICS)]
         [dns=>ports/unsorted
          (foldl (λ (dns port dns=>ports)
                   (hash-update dns=>ports dns 
                                (λ (existing-ports) 
                                  (cons port existing-ports))
                                (list port)))
                 (hash)
                 (map car known-island-addresses)
                 (map cdr known-island-addresses))])
    (for/hash ([(dns ports/unsorted) dns=>ports/unsorted])
      (values (bytes->string/utf-8 dns) (map number->string (sort ports/unsorted <))))))

;;; -------

(struct video-gui-client (gui controller@))

(define top-width 700)
(define top-height 900)

(define FONT (make-object font% 12 'swiss))

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

(define BYTES-PER-PIXEL 4)

; video-playback: used to track the metadata and state of an individual in-play, displayed video.
(struct video-playback (buffer name w h)) ; bytes? curl integer? integer?

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
                           ;[min-width 80]
                           ;[min-height 10]
                           [auto-resize #t]
                           [font FONT]))
  
  
  (define button-panel (new horizontal-panel%
                            [parent the-window]
                            [alignment '(center top)]))  
  
  (define dns-choice (new choice% 
                          [parent button-panel]
                          [label #f]
                          [choices (hash-keys dns=>ports)]
                          [font FONT]
                          [callback (λ (c e)
                                      (send port-choice clear)
                                      (map (λ (port) (send port-choice append port)) 
                                           (hash-ref dns=>ports (send c get-string-selection))))]))
  
  (define port-choice (new choice%
                           [parent button-panel]
                           [label #f]
                           [choices (hash-ref dns=>ports (send dns-choice get-string-selection))]
                           [font FONT]))
  
  (define cp-button (new button%
                         [parent button-panel]
                         [label "Share this session"]
                         [font FONT]
                         [callback (λ (btn ctrlevt)
                                     (cb (CopyActor/new
                                          (send dns-choice get-string-selection) 
                                          (string->number (send port-choice get-string-selection)))))]))
  
  (define mv-button (new button%
                         [parent button-panel]
                         [label "Move this session"]
                         [font FONT]
                         [callback (λ (btn ctrlevt)
                                     (map (λ (child)
                                            (send frame delete-child child))
                                          (send frame get-children))
                                     (send frame show #f)
                                     (send frame enable #f)
                                     (cb (Quit/MV/new (send dns-choice get-string-selection) 
                                                      (string->number (send port-choice get-string-selection))))
                                     (send frame on-close))]))
  
  (define top-panel (new video-top-panel%
                         [parent the-window]
                         [addressbar address-bar]
                         [alignment '(left top)]
                         [stretchable-width #t]
                         [stretchable-height #t]))
  
  (define small-panel (new small-video-panel%
                           [parent top-panel]
                           [main-panel top-panel]
                           [hostfield dns-choice]
                           [portfield port-choice]
                           [alignment '(left top)]
                           [style '(auto-vscroll auto-hscroll)]
                           ;[min-height 200]
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
  (class vertical-panel% [init addressbar]
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
  (class horizontal-panel% [init main-panel hostfield portfield]
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
      (define canvas-container 
        (new vertical-panel% 
             [parent this]
             [alignment '(left top)]))
      
      (define new-canvas-width (round (/ (video-playback-w v) 4)))
      (define new-canvas-height (round (/ (video-playback-h v) 4)))
      
      ;; the new SMALL video canvas.
      (define cnvs
        (new small-video-canvas%
             [parent canvas-container]
             [cv v]
             [toppanel top-panel]
             [addressbar abr]
             [min-width new-canvas-width]
             [min-height new-canvas-height]
             [enabled #f]))
      
      ;; all control buttons go in this container.
      (define control-container 
        (new horizontal-panel% 
             [parent canvas-container] 
             [alignment '(left top)]))
      
      (define (do-unsub)
        (evt-cb (RemoveCURL/new (video-playback-name v)))
        
        (send cnvs stop)
        ;(send canvas-container delete-child cnvs)
        ;(send vertp delete-child unsub)
        ;(send vertp delete-child cpy)
        ;(send vertp delete-child mv)
        (send canvas-container delete-child control-container)
        (send this delete-child canvas-container)
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
      
      (define (do-cpy)
        (evt-cb (CopyChild/new (video-playback-name v)
                               (send host-field get-string-selection)
                               (string->number (send port-field get-string-selection)))))
      
      (define (do-flow-source-move)
        (evt-cb (FwdBackward/new (Quit/MV/new (send host-field get-string-selection)
                                              (string->number (send port-field get-string-selection)))
                                 (video-playback-name v))))
      
      (define flow-control-choices
        (new choice%
             [parent control-container]
             [label #f]
             [choices '("Remove flow"
                        "Share flow sink"
                        "Move flow sink"
                        "Move flow src"
                        "Create PIP"
                        "Split PIP"
                        "Swap PIP")]
             [min-width (inexact->exact (round (* 0.90 new-canvas-width)))]
             [stretchable-width #f]
             [font FONT]))
      
      (define do-it-button
        [new button%
             [label "Do it!"]
             [font FONT]
             [parent control-container]
             [min-width (inexact->exact (round (* 0.10 new-canvas-width)))]
             [stretchable-width #f]
             [callback (λ (c e)
                         (let ([the-choice (send flow-control-choices get-string-selection)])
                           (case the-choice
                             [("Remove flow") (do-unsub)]
                             [("Share flow sink") (do-cpy)]
                             [("Move flow sink") (do-cpy) (do-unsub)]
                             [("Move flow src") (do-flow-source-move)]
                             [("Create PIP") (pip-activation-evt evt-cb (video-playback-name v))]
                             [("Split PIP") (evt-cb (InitiateBehavior/new 'split (video-playback-name v)))]
                             [("Swap PIP") (evt-cb (InitiateBehavior/new 'toggle-major/minor (video-playback-name v)))]
                             [else #f])))]])
      
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
    (inherit suspend-flush resume-flush
             refresh is-enabled? with-gl-context swap-gl-buffers)
    
    (suspend-flush)
    
    (define myvideo cv)
    (define w (video-playback-w myvideo))
    (define h (video-playback-h myvideo))
    (define buffer (video-playback-buffer myvideo))
    (refresh)
    
    (define/public (stop)
      (set! myvideo #f)
      (set! buffer #f)
      (send refresher stop))
    
    (define/public (same-video? v)
      (eq? myvideo v))
    
    (define is-init? #f)
    (define update! (make-updater w h buffer))
    
    (define/private (gl-init)
      (with-handlers ([exn:fail? (λ (e) (displayln e) e)])
        (with-gl-context
         (λ ()
           (glEnable GL_TEXTURE_2D)
           (glPixelStorei GL_UNPACK_ALIGNMENT 1)
           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
           (glTexImage2D GL_TEXTURE_2D ; target
                         0 GL_RGB
                         w h
                         0 ; border
                         GL_BGRA GL_UNSIGNED_BYTE buffer)))
        (update!)
        (set! is-init? #t)))
    
    (define/override (on-paint)
      (when is-init?
        (with-handlers ([exn:fail? (λ (e) (displayln e))])
          (with-gl-context update!)
          (swap-gl-buffers))))
    
    (gl-init)
    (resume-flush)
    
    (define refresher
      (new timer% 
           [notify-callback (λ () (refresh))]
           [interval 50] 
           [just-once? #f]))))

; small-video-canvas%: used for the preview panes at the bottom of the screen.
(define small-video-canvas%
  (class* canvas% (stoppable<%>)
    [init cv addressbar toppanel]
    (super-new [style '(gl)]
               [stretchable-width #f]
               [stretchable-height #f])
    (inherit suspend-flush resume-flush
             refresh is-enabled? with-gl-context swap-gl-buffers)
    
    (suspend-flush)
    
    (define top-panel toppanel)
    (define myvideo cv)
    (define buffer (video-playback-buffer myvideo))
    (define actual-w (video-playback-w cv))
    (define actual-h (video-playback-h cv))
    (refresh)
    
    (define is-init? #f)
    (define update! (make-updater actual-w actual-h buffer))
    
    (define/private (gl-init)
      (with-handlers ([exn:fail? (λ (e) (displayln e) e)])
        (with-gl-context
         (λ ()
           (glEnable GL_TEXTURE_2D)
           (glPixelStorei GL_UNPACK_ALIGNMENT 1)
           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
           (glTexImage2D GL_TEXTURE_2D ; target
                         0 GL_RGB
                         actual-w actual-h
                         0 ; border
                         GL_BGRA GL_UNSIGNED_BYTE buffer)))
        (update!)
        (set! is-init? #t)))
    
    (define/override (on-paint)
      (when is-init?
        (with-handlers ([exn:fail? (λ (e) (displayln e))])
          (with-gl-context update!)
          (swap-gl-buffers))))
    
    (define/public (stop)
      (set! myvideo #f)
      (set! buffer #f)
      (send refresher stop))
    
    (define/override (on-event e)
      (when (and (send e button-down?) 
                 (not (send top-panel showing? myvideo))) ; is the video not already showing?
        (promote-self))) ; if not showing, show it
    
    (define/public (promote-self)
      (send top-panel swap-focused-video myvideo))
    
    (gl-init)
    (resume-flush)
    
    (define refresher
      (new timer% 
           [notify-callback (λ () (refresh))]
           [interval 250] 
           [just-once? #f]))))

;;; -------------
;;; OpenGL stuff.
;;; -------------
;; scale, do some ops then reverse the scaling on the way out.
(define-syntax-rule (with-gl-scaling x y z e ...)
  (dynamic-wind (λ () (glScalef x y z))
                (λ () e ...)
                (λ () (glScalef (1 . / . x) (1 . / . y) (1 . / . z)))))

; run glBegin, do some GL operations then clean up on the way out.
(define-syntax-rule (with-gl-begin mode gl-op ...)
  (dynamic-wind (λ () (glBegin mode))
                (λ () gl-op ...)
                glEnd))

(define (make-updater w h buffer)
  (λ ()
    (glTexSubImage2D GL_TEXTURE_2D ; target
                     0 0 0 ; level x-offset y-offset
                     w h
                     GL_BGRA GL_UNSIGNED_BYTE buffer)
    (with-gl-scaling 
     -1.0 1.0 1.0
     (with-gl-begin 
      GL_QUADS
      (glTexCoord2i 0 0) (glVertex3i 1 1 0)
      (glTexCoord2i 0 1) (glVertex3i 1 -1 0)
      (glTexCoord2i 1 1) (glVertex3i -1 -1 0)
      (glTexCoord2i 1 0) (glVertex3i -1 1 0)))))