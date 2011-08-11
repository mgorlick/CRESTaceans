#lang racket/base

(require racket/place
         racket/match)

(provide (except-out (all-defined-out)
                     client/new-video-gui
                     client/video-gui-add-video!
                     client/video-playback-buffersize
                     client/video-playback-buffer)
         (rename-out [client/new-video-gui new-video-gui]
                     [client/video-gui-add-video! video-gui-add-video!]
                     [client/video-playback-buffersize video-playback-buffersize]
                     [client/video-playback-buffer video-playback-buffer]))

; #('closed-feed <curl>)
(define (gui-message-closed-feed? m)
  (and (vector? m)
       (= 2 (vector-length m))
       (equal? (vector-ref m 0) 'closed-feed)))

; #('closed-window)
(define (gui-message-closed-window? m)
  (and (vector? m)
       (= 1 (vector-length m))
       (equal? (vector-ref m 0) 'closed-window)))

; #('pip-on <main curl> <sub curl>)
(define (gui-message-pip-on? m)
  (and (vector? m)
       (= 3 (vector-length m))
       (equal? (vector-ref m 0) 'pip-on)))

; #('pip-off <main curl> <sub curl>)
(define (gui-message-pip-off? m)
  (and (vector? m)
       (= 2 (vector-length m))
       (equal? (vector-ref m 0) 'pip-off)))

(define (gui-message-closed-feed-curl m)
  (vector-ref m 1))
(define (gui-message-pip-on-main-curl m)
  (vector-ref m 1))
(define (gui-message-pip-on-sub-curl m)
  (vector-ref m 2))
(define (gui-message-pip-off-main-curl m)
  (vector-ref m 1))

; places interface

(struct video-gui-client (place actor-thread))

(define (client/new-video-gui w h)
  (define pls (dynamic-place "gui-backend.rkt" 'gui-main))
  (place-channel-put pls (list 'new-video-gui w h))
  (video-gui-client pls (current-thread)))

(define (client/video-gui-add-video! c w h name)
  (place-channel-put/get (video-gui-client-place c)
                         (list 'add-video w h name)))

(define (gui-thread-receive c)
  (define tre (thread-receive-evt))
  (define syncres (sync tre (video-gui-client-place c)))
  (cond [(equal? syncres tre) (thread-receive)]
        [else (cons syncres #f)]))

(define (client/video-playback-buffersize p)
  (bytes-length p))

(define (client/video-playback-buffer p)
  p)