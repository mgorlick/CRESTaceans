#lang racket/base

(require racket/class
         racket/gui/base
         racket/function
         racket/contract
         ffi/vector
         "message-types.rkt"
         "../../peer/src/api/config.rkt"
         "motile-imports.rkt"
         "../../Motile/actor/curl.rkt"
         "../../Motile/actor/send.rkt"
         (planet "rgl.rkt" ("stephanh" "RacketGL.plt")))

(provide (rename-out [client/new-video-gui new-video-gui]
                     [client/video-gui-add-video! video-gui-add-video!])
         video-gui-clear-buffer!
         video-gui-update-buffer!)

; hash table: string (dns name) => list of string (port)
(define dns=>ports
  (let* ([known-island-addresses (hash-keys PUBLICS)]
         [dns=>ports/unsorted
          (foldl (λ (dns port dns=>ports)
                   (hash-update dns=>ports dns (curry cons port) (list port)))
                 (hash)
                 (map car known-island-addresses)
                 (map cdr known-island-addresses))])
    (for/hash ([(dns ports/unsorted) dns=>ports/unsorted])
      (values (bytes->string/utf-8 dns) (map number->string (sort ports/unsorted <))))))

#|(define-motile-procedure horizontal-flip ; do not use! slow as molasses!
  '(lambda (content w h)
     (define l (bytes-length content))
     (define expected (/ (* 3 w h) 2))
     (define yl 0)
     (define ul (* 2 (/ l 3)))
     (define vl (/ l 6))
     (define o (open-output-bytes))
     (let do-planes ([num-rows*    (list h  (/ h 2)   (/ h 2))]
                     [sopr*        (list w  (/ w 2)   (/ w 2))] ; size of plane row
                     [src-offset*  (list yl (+ yl ul) (+ yl ul vl))])
       (cond [(or (null? num-rows*) (null? sopr*) (null? src-offset*))
              ; done iterating through all the planes - produce final content
              (get-output-bytes o)]
             [else
              ; do a single plane
              (let ([num-rows (car num-rows*)] [sopr (car sopr*)] [src-offset (car src-offset*)])
                (let do-single-plane ([row 0])
                  (define row-at (+ src-offset (* row sopr)))
                  (cond [(= num-rows row) (void)]
                        [else 
                         (let write-a-row ([row-byte-offset (sub1 sopr)])
                           (cond [(= row-byte-offset -1)
                                  (do-single-plane (add1 row))]
                                 [else 
                                  (write-bytes content o 
                                               (+ row-at row-byte-offset)
                                               (+ row-at row-byte-offset 1))
                                  (write-a-row (sub1 row-byte-offset))]))])))
              (do-planes (cdr num-rows*) (cdr sopr*) (cdr src-offset*))]))))|#
(define-motile-procedure noise
  '(lambda (content w h)
     (define l (bytes-length content))
     (define o (open-output-bytes))
     (define num-rows h)
     (define sopr w)
     (do ([row 0 (+ 2 row)]) [(<= num-rows row)]
       (define row-at (* row sopr))
       (define the-posn (random (* 2 sopr)))
       (write-bytes (bytes-set content the-posn (random 256) row-at (+ row-at (* 2 sopr))) o))
     (write-bytes content o (* 2 (/ l 3))) ; copy U and V
     (get-output-bytes o)))
(define-motile-procedure vertical-flip
  '(lambda (content w h)
     (define l (bytes-length content))
     (define yl (* 2 (/ l 3)))
     (define ul (/ l 6))
     (define vl (/ l 6))
     (get-output-bytes
      (foldl (lambda (num-rows sopr src-offset o)
               (do ([row 0 (add1 row)]) [(= num-rows row) o]
                 (define row-at (- src-offset (* (add1 row) sopr)))
                 (write-bytes content o row-at (+ row-at sopr))))
             (open-output-bytes)
             (list h (/ h 2) (/ h 2))
             (list w (/ w 2) (/ w 2))
             (list yl (+ yl ul) (+ yl ul vl))))))
(define-motile-procedure greyscale
  '(lambda (content w h)
     (bytes-set-all content 128 (* 2 (/ (bytes-length content) 3)))))

;;; -------

(struct video-gui-client (gui controller@))

(define top-width 750)
(define top-height 750)

(define FONT (make-object font% 12 'swiss))

;;;;;;;; ---------------------------

(define BYTES-PER-PIXEL 4)

; video-playback: used to track the metadata and state of an individual in-play, displayed video.
(struct video-playback (buffer name w h [fx #:mutable])) ; bytes? curl integer? integer? (listof string?)

(define (video-playback-buffersize v)
  (bytes-length (video-playback-buffer v)))

(define (make-video-playback width height name fx)
  (define buffer (make-bytes (* BYTES-PER-PIXEL width height)))
  (video-playback buffer name width height fx))

(struct video-gui (frame top-panel address-bar
                         small-video-panel) #:transparent #:mutable)

(define/contract (video-gui-add-video! g w h name fx evt-cb)
  (video-gui? number? number? curl? (listof string?) procedure? . -> . video-playback?)
  (define nv (make-video-playback w h name fx))
  (define address-bar (video-gui-address-bar g))
  (define small-panel (video-gui-small-video-panel g))
  (send small-panel add-video-canvas nv address-bar)
  nv)

(define (video-gui-clear-buffer! v)
  (bytes-fill! (video-playback-buffer v) 0))

(define (video-gui-update-buffer! v frame fx)
  (bytes-copy! (video-playback-buffer v) 0 frame)
  (set-video-playback-fx! v fx))

; ---------------------------------------------------

(define (make-notify-controller-callback controller@)
  (λ (m)
    (curl/send controller@ (remote/new m null #f))))

(define (client/new-video-gui controller@)
  (define the-actor-thread (current-thread))
  (parameterize ([current-eventspace (make-eventspace)])
    (video-gui-client (new-video-gui top-width top-height (make-notify-controller-callback controller@))
                      controller@)))

(define/contract (client/video-gui-add-video! client w h fx name)
  (video-gui-client? number? number? (listof string?) curl? . -> . video-playback?)
  (parameterize ([current-eventspace (make-eventspace)])
    (video-gui-add-video! (video-gui-client-gui client)
                          w h name fx 
                          (make-notify-controller-callback (video-gui-client-controller@ client)))))


; ---------------------------------


(define (new-video-gui overall-width overall-height cb)
  
  (define frame (new closeable-frame% 
                     [label ""]
                     [min-width overall-width]
                     [min-height overall-height]
                     [style '(no-resize-border)]
                     [callback (λ () (cb (Quit/new)))]))
  
  (define the-window (new vertical-panel% [parent frame]))
  
  (define address-bar (new message%
                           [parent the-window]
                           [label "No flow"]
                           [min-width overall-width]
                           [min-height 12]
                           [auto-resize #f]
                           [font FONT]))
  
  ;; menu for operating on whatever flow is currently selected.
  
  (define menu-bar (new menu-bar% [parent frame]))
  
  (define flow-menu (new menu% [parent menu-bar] [label "Flow..."]))
  (define unsubscribe (new menu-item% 
                           [parent flow-menu] [label "Unsubscribe"] [callback (λ _ (do-unsub))]))
  (define share-sink (new menu-item%
                          [parent flow-menu] [label "Share sink"] [callback (λ _ (do-cpy))]))
  (define move-src (new menu-item%
                        [parent flow-menu] [label "Move source"] [callback (λ _ (do-flow-source-move))]))
  (define do-greyscale (new checkable-menu-item%
                            [parent flow-menu]
                            [label "Greyscale"]
                            [callback (λ (i e) (if (send i is-checked?)
                                                   (do-fx greyscale "greyscale")
                                                   (rmv-fx greyscale "greyscale")))]))
  (define do-noise (new checkable-menu-item%
                        [parent flow-menu]
                        [label "Noise"]
                        [callback (λ (i e) (if (send i is-checked?)
                                               (do-fx noise "noise")
                                               (rmv-fx noise "noise")))]))
  (define do-vertical-flip (new checkable-menu-item%
                                [parent flow-menu]
                                [label "Vertical flip"]
                                [callback (λ (i e) (if (send i is-checked?)
                                                       (do-fx vertical-flip "vertical-flip")
                                                       (rmv-fx vertical-flip "vertical-flip")))]))
  #|(define do-hflip (new checkable-menu-item%
                        [parent flow-menu]
                        [label "Horizontal flip"]
                        [callback (λ (i e) (if (send i is-checked?)
                                               (do-fx horizontal-flip "horizontal-flip")
                                               (rmv-fx horizontal-flip "horizontal-flip")))]))|#
  
  (define compose-menu (new menu% [parent menu-bar] [label "Compose..."]))
  (define set-major-pip (new menu-item%
                             [parent compose-menu] [label "Set PIP major flow"] 
                             [callback (λ _ 
                                         (do-pip-major-selection!)
                                         (when (pip-ready-to-go?) (send launch-pip enable #t)))]))
  (define set-minor-pip (new menu-item%
                             [parent compose-menu] [label "Set PIP minor flow"] 
                             [callback (λ _ 
                                         (do-pip-minor-selection!)
                                         (when (pip-ready-to-go?) (send launch-pip enable #t)))]))
  (define launch-pip 
    (let ([item (new menu-item%
                     [parent compose-menu] [label "Launch PIP"]
                     [callback (λ _
                                 (maybe-launch-pip!)
                                 (send set-major-pip enable #t)
                                 (send set-minor-pip enable #t)
                                 (reset-pip-selections!))])])
      (send item enable #f)
      item))
  (define swap-pip-order (new menu-item%
                              [parent compose-menu] [label "Swap PIP ordering"]
                              [callback (λ _(cb (InitiateBehavior/new 'split (get-current-flow-curl))))]))
  (define split-pip-into-components (new menu-item%
                                         [parent compose-menu] [label "Split PIP into component flows"]
                                         [callback (λ _ (cb (InitiateBehavior/new 'toggle-major/minor (get-current-flow-curl))))]))
  
  (define session-menu (new menu% [parent menu-bar] [label "Session..."]))
  (define share-session (new menu-item%
                             [parent session-menu] [label "Share"] [callback (λ _ (do-share-session))]))
  (define move-session (new menu-item%
                            [parent session-menu] [label "Move"] [callback (λ _ (do-move-session))]))
  
  ;; context for menu operations: dns/port combinations naming islands to send computations to.
  
  (define button-panel (new horizontal-panel%
                            [parent the-window]
                            [alignment '(center top)]
                            [spacing 10]
                            [stretchable-width #f]
                            [stretchable-height #f]))
  (new message% [parent button-panel] [label "Island at:"] [vert-margin 10] [font FONT])
  (define dns-choice (new choice% 
                          [parent button-panel]
                          [label #f]
                          [choices (sort (hash-keys dns=>ports) string<=?)]
                          [font FONT]
                          [callback (λ (c e)
                                      (send port-choice clear)
                                      (map (λ (port) (send port-choice append port))
                                           (hash-ref dns=>ports (send dns-choice get-string-selection) '())))]))
  (define port-choice (new choice%
                           [parent button-panel]
                           [label #f]
                           [choices (hash-ref dns=>ports (send dns-choice get-string-selection) '())]
                           [font FONT]))
  
  ;; trigger operations on the currently selected flow.
  
  (define (get-current-flow-curl)
    (send big-video-panel get-current-canvas-curl))
  (define (current-selected-host)
    (send dns-choice get-string-selection))
  (define (current-selected-port)
    (send port-choice get-string-selection))
  
  (define (do-fx f label)
    (cb (AddFx/new f label (get-current-flow-curl))))  
  (define (rmv-fx f label)
    (cb (RemoveFx/new f label (get-current-flow-curl))))
  
  (define (do-cpy)
    (cb (CopyChild/new (get-current-flow-curl) (current-selected-host) (string->number (current-selected-port)))))
  
  (define (do-flow-source-move)
    (cb (FwdBackward/new (Quit/MV/new (current-selected-host) (string->number (current-selected-port)))
                         (get-current-flow-curl))))
  
  (define (do-unsub)
    (cb (RemoveCURL/new (get-current-flow-curl)))
    (send address-bar set-label "No flow")
    (send big-video-panel tell-unsubbed small-video-panel))
  
  (define pip-major-curl #f)
  (define pip-minor-curl #f)
  
  (define (pip-ready-to-go?)
    (and pip-major-curl pip-minor-curl))
  (define (reset-pip-selections!)
    (set! pip-major-curl #f)
    (set! pip-minor-curl #f))
  (define (do-pip-major-selection!)
    (unless pip-major-curl (set! pip-major-curl (get-current-flow-curl))))
  (define (do-pip-minor-selection!)
    (unless pip-minor-curl (set! pip-minor-curl (get-current-flow-curl))))
  (define (maybe-launch-pip!)
    (and (and pip-major-curl pip-minor-curl)
         (cb (PIPOn/new pip-major-curl pip-minor-curl))
         (reset-pip-selections!)
         #t))
  
  (define (do-share-session)
    (cb (CopyActor/new (current-selected-host) (string->number (current-selected-port)))))
  
  (define (do-move-session)
    (map (λ (child)
           (send frame delete-child child))
         (send frame get-children))
    (send frame show #f)
    (send frame enable #f)
    (cb (Quit/MV/new (current-selected-host) (string->number (current-selected-port))))
    (send frame on-close))
  
  ;; panels for holding actual video output.
  
  (define top-panel (new vertical-panel% [parent the-window]))
  
  (define all-fx `(("greyscale" . ,do-greyscale)
                   ("vertical-flip" . ,do-vertical-flip)
                   ("noise" . ,do-noise)))
  
  (define video-top-panel%
    (class vertical-panel%
      (super-new)
      (inherit add-child delete-child get-parent)
      (define current-canvas #f)
      (define current-canvas-sema (make-semaphore 1))
      (define current-video #f)
      (define (clear-current-canvas)
        (when current-canvas 
          (delete-child current-canvas)
          (send current-canvas show #f)
          (send current-canvas enable #f) 
          (send current-canvas stop)))
      (define/public (tell-unsubbed target)
        (send target unsubbed current-video))
      (define/public (get-current-canvas-curl)
        (call-with-semaphore current-canvas-sema
                             (λ () (and current-canvas (send current-canvas get-curl)))))
      (define/public (showing? v)
        (eq? current-video v))
      (define/public (no-videos)
        (call-with-semaphore current-canvas-sema clear-current-canvas))
      (define/public (swap-focused-video v)
        (call-with-semaphore 
         current-canvas-sema 
         (λ () 
           (set! current-video v)
           (for-each (λ (fxr)
                       (send (cdr fxr) check 
                             (if (member (car fxr) (video-playback-fx v))
                                 #t #f)))
                     all-fx)
           (send do-vertical-flip check (if (member "vertical-flip" (video-playback-fx v)) #t #f))
           (send address-bar set-label (format "~a" (curl/pretty (video-playback-name v))))
           (send (get-parent) min-width (video-playback-w v))
           (send (get-parent) min-height (video-playback-h v))
           (clear-current-canvas)
           (set! current-canvas
                 (new video-canvas% 
                      [parent this] [cv v] [vert-margin 10]  [horiz-margin 10] [enabled #f]
                      [min-width (video-playback-w v)] [min-height (video-playback-h v)]))
           (send current-canvas enable #t))))))
  
  (define big-video-panel (new video-top-panel%
                               [parent top-panel]
                               [alignment '(left top)]
                               [stretchable-width #f]
                               [stretchable-height #f]
                               [border 10]
                               [enabled #f]))
  
  (define small-video-panel (new small-video-panel%
                                 [parent top-panel]
                                 [bvp big-video-panel]
                                 [alignment '(left top)]
                                 [style '(auto-vscroll auto-hscroll)]
                                 [spacing 10]
                                 [border 10]
                                 [enabled #f]))
  
  ; initialize the last few uninitialized elements.
  (send small-video-panel enable #t)
  (send big-video-panel enable #t)
  (send frame show #t)
  (video-gui frame big-video-panel address-bar small-video-panel))

; ---------------------------

(define closeable-frame%
  (class frame% [init callback]
    (super-new)
    (define on-close-callback callback)
    (define/augment (on-close)
      (on-close-callback))))

; ---------------------------

; small-video-panel: holds small-video-canvas%es as they are created.
(define small-video-panel%
  (class horizontal-panel% [init bvp]
    (super-new)
    (inherit get-parent get-children)
    (define big-video-panel bvp)
    
    (define/public (unsubbed v)
      (for/or ([small-canvas (get-children)])
        (cond [(send small-canvas same-video? v)
               (send small-canvas stop)
               (send this delete-child small-canvas)
               (if (null? (get-children))
                   (send big-video-panel no-videos)
                   (send (car (get-children)) promote-self))
               #t]
              [else #f])))
    
    (define/public (add-video-canvas v abr)
      (define new-canvas-width (round (/ (video-playback-w v) 4)))
      (define new-canvas-height (round (/ (video-playback-h v) 4)))
      
      ;; the new SMALL video canvas.
      (define cnvs
        (new small-video-canvas%
             [parent this]
             [cv v]
             [toppanel big-video-panel]
             [addressbar abr]
             [min-width new-canvas-width]
             [min-height new-canvas-height]
             [enabled #f]))
      
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
    
    (define/public (get-curl)
      (video-playback-name myvideo))
    
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
        (with-handlers ([exn:fail? displayln])
          (with-gl-context update!)
          (swap-gl-buffers))))
    
    (gl-init)
    
    (define refresher
      (new timer% 
           [notify-callback (λ () (refresh))]
           [interval 50] 
           [just-once? #f]))
    (resume-flush)))

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
    
    (define/public (same-video? v)
      (eq? myvideo v))
    
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
        (with-handlers ([exn:fail? displayln])
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
    
    (define refresher
      (new timer% 
           [notify-callback (λ () (refresh))]
           [interval 250] 
           [just-once? #f]))
    (resume-flush)))

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