#lang racket/base

(require "ws.rkt"
         racket/async-channel)

(define (open-a-tab/synch)
  (define ac (make-async-channel))
  (define (wait-for-response)
    (async-channel-get ac))  
  (define s (open-a-tab (λ (json) (async-channel-put ac json))))
  (values s wait-for-response))

(define-values (s get-last-response) (open-a-tab/synch)) 
(ui-wait-for-readiness s)

(define w 320)
(define h 240)

(define (new-data)
  (define b# (make-bytes (* w h 4)))
  (for ([i (in-range (* w h 4))])
    (bytes-set! b# i (random 256)))
  b#)

(ui-send! s (new-button "Foo"))
(define session-menu (new-menu "Session..."))
(define file-menu (new-menu "File..."))
(ui-send! s file-menu)
(ui-send! s session-menu)
(ui-send! s (new-menu-item "Unsubscribe" file-menu "alert(\"Unsubscribe pressed\");"))
(ui-send! s (new-menu-item "Share sink" file-menu "alert(\"Share Sink pressed\");"))
(ui-send! s (new-menu-item "Move source" file-menu "alert(\"Move Source pressed\");"))
(ui-send! s (new-menu-item "Greyscale" file-menu "alert(\"Greyscale pressed\");"))
(ui-send! s (new-menu-item "Vertical flip" file-menu "alert(\"Vertical Flip pressed\");"))
(ui-send! s (new-menu-item "Share" session-menu "alert(\"Share pressed\");"))
(ui-send! s (new-menu-item "Move" session-menu "alert(\"Move pressed\");"))
(ui-send! s (new-dropdown "ips" "ips.json"))
(define c (new-canvas "mainvideo" w h))
(ui-send! s c)
(let loop ()
  ;(sleep 1/20)
  (ui-send! s (update-canvas-contents c (new-data)))
  (loop))
#| 
(ui-send! s (update-canvas-contents c (make-bytes (* 4 500) 255)))
|#