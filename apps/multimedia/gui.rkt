#lang racket/base

(require racket/place
         unstable/match
         "message-types.rkt")

(provide (except-out (all-defined-out)
                     client/new-video-gui
                     client/video-gui-add-video!)
         (rename-out [client/new-video-gui new-video-gui]
                     [client/video-gui-add-video! video-gui-add-video!]))

; Q: Why are these gussied-up vectors instead of motile structs?
; A: because compiled Motile structs have procedures inside them and you can't
; send a procedure over a place channel.
; #('closed-feed <curl>)
(define (gui-message-closed-feed? m)
  (match? m (vector 'closed-feed _)))

; #('closed-window)
(define (gui-message-closed-window? m)
  (match? m (vector 'closed-window)))

; #('pip-on <main curl> <sub curl>)
(define (gui-message-pip-on? m)
  (match? m (vector 'pip-on _ _)))

; #('pip-off <main curl> <sub curl>)
(define (gui-message-pip-off? m)
  (match? m (vector 'pip-off _ _)))

; #('mv <host> <port>)
(define (gui-message-mv? m)
  (match? m (vector 'mv _ _)))

; #('cp <host> <port>)
(define (gui-message-cp? m)
  (match? m (vector 'cp _ _)))

; #('cp <curl> <host> <port>)
(define (gui-message-cp-child? m)
  (match? m (vector 'cp-child _ _ _)))

(define (gui-message-closed-feed-curl m)
  (vector-ref m 1))

(define (selective-translate m)
  (cond [(gui-message-mv? m) (Quit/MV (vector-ref m 1) (vector-ref m 2))]
        [(gui-message-cp? m) (CP (vector-ref m 1) (vector-ref m 2))]
        [(gui-message-closed-window? m) (Quit)]
        [(gui-message-cp-child? m) (CP-child (vector-ref m 1) (vector-ref m 2) (vector-ref m 3))]
        [(gui-message-pip-on? m) (PIPOn (vector-ref m 1) (vector-ref m 2))]
        [else m]))

; places interface

(struct video-gui-client (place actor-thread))

(define main-pls (dynamic-place "gui-backend.rkt" 'gui-main))

(define (client/new-video-gui w h)
  (place-channel-put main-pls (list 'new-video-gui w h))
  (define new-pls (place-channel-get main-pls))
  (video-gui-client new-pls (current-thread)))

(define (shutdown-gui c)
  (place-kill (video-gui-client-place c)))

(define (client/video-gui-add-video! c w h name)
  (place-channel-put/get (video-gui-client-place c)
                         (list 'add-video w h name)))

(define (gui-thread-receive c)
  (define tre (thread-receive-evt))
  (define syncres (sync tre (video-gui-client-place c)))
  (cond [(equal? syncres tre) (thread-receive)]
        [else (cons (selective-translate syncres) #f)]))