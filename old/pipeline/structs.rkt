#lang racket/base

(provide (all-defined-out))

;; a disposal is a thunk which 'disposes' of the frame data
;; contract: a downstream consumer must execute disposal thunk
;; when it is the last consumer in the chain to use the buffer
;; and the FrameBuffer manufacturer guarantees that executing disposal
;; will eventually requeue the buffer for later reuse
(struct FrameBuffer (data size disposal ts) #:transparent)
(struct VideoParams (width height fpsNum fpsDen) #:transparent)

;; Motile code gets confused when trying to run ((FrameBuffer-disposal v))
(define (dispose-FrameBuffer fb)
  ((FrameBuffer-disposal fb)))