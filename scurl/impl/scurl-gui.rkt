#lang scheme/gui

;;==========================================================================
;;===                Code generated with MrEd Designer 3.7               ===
;;===                 http://mred-designer.origo.ethz.ch                 ===
;;==========================================================================

;;; Call (scurl-impl-init) with optional arguments to this module

(require
 framework
 )

(provide scurl-impl-init main-frame mainTab)

(define (label-bitmap-proc l)
  (let ((label (first l)) (image? (second l)) (file (third l)))
    (or (and image? (or (and file (let ((bmp (make-object bitmap% file 'unknown/mask))) (and (send bmp ok?) bmp))) "<Bad Image>")) label)))

(define (list->font l)
  (with-handlers
   ((exn:fail? (λ (e) (send/apply the-font-list find-or-create-font (cons (first l) (rest (rest l)))))))
   (send/apply the-font-list find-or-create-font l)))

(define scurl-impl #f)
(define main-frame #f)
(define mainTab #f)
(define control-tab #f)
(define (scurl-impl-init
         #:main-frame-label
         (main-frame-label "SCURL Test")
         #:main-frame-width
         (main-frame-width 800)
         #:main-frame-height
         (main-frame-height 500)
         #:control-tab-label
         (control-tab-label "Controls"))
  (set! main-frame
    (new
     frame%
     (parent scurl-impl)
     (label main-frame-label)
     (width main-frame-width)
     (height main-frame-height)
     (x #f)
     (y #f)
     (style '())
     (enabled #t)
     (border 0)
     (spacing 0)
     (alignment (list 'center 'top))
     (min-width 70)
     (min-height 30)
     (stretchable-width #t)
     (stretchable-height #t)))
  (set! mainTab
    (new
     (class tab-panel%
       (super-new)
       (define single-panel (new panel:single% (parent this)))
       (define/public (get-single-panel) single-panel)
       (define child-panels '())
       (define/public (add-child-panel p label) (set! child-panels (append child-panels (list p))) (send this append label))
       (define/public (active-child n) (send single-panel active-child (list-ref child-panels n))))
     (parent main-frame)
     (choices (list))
     (callback (λ (tp e) (send tp active-child (send tp get-selection))))
     (style '())
     (enabled #t)
     (vert-margin 0)
     (horiz-margin 0)
     (border 0)
     (spacing 0)
     (alignment (list 'center 'center))
     (min-width 0)
     (min-height 0)
     (stretchable-width #t)
     (stretchable-height #t)))
  (set! control-tab
    (new
     (class vertical-panel%
       (init parent)
       (init-field label)
       (super-new (parent (send parent get-single-panel)))
       (send parent add-child-panel this label))
     (parent mainTab)
     (label control-tab-label)
     (style '())
     (enabled #t)
     (vert-margin 0)
     (horiz-margin 0)
     (border 0)
     (spacing 0)
     (alignment (list 'left 'center))
     (min-width 0)
     (min-height 0)
     (stretchable-width #t)
     (stretchable-height #t)))
  (send main-frame show #t))
