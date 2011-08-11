#lang racket/base

(require racket/place)

(provide (rename-out [client/new-video-gui new-video-gui]
                     [client/video-gui-add-video! video-gui-add-video!]
                     [client/video-playback-buffersize video-playback-buffersize]
                     [client/video-playback-buffer video-playback-buffer]))

; places interface

(define (client/new-video-gui w h)
  (define pls (dynamic-place "gui-backend.rkt" 'gui-main))
  (place-channel-put pls (list 'new-video-gui w h))
  pls)

(define (client/video-gui-add-video! pls w h name)
  (place-channel-put/get pls (list 'add-video w h (string->immutable-string name))))

(define (client/video-playback-buffersize p)
  (bytes-length p))

(define (client/video-playback-buffer p)
  p)