#lang racket/base

(require "../../Motile/struct.rkt")
(provide (all-defined-out))

(define-motile-struct AddCURL [curl])
(define-motile-struct RemoveCURL [curl])
(define-motile-struct AddDecodedVideo [w h decodercurl])
(define-motile-struct Quit [])
(define-motile-struct Quit/MV [host port])
(define-motile-struct CP [host port])
(define-motile-struct CP-child [curl host port])
(define-motile-struct Frame [data timestamp])
(define-motile-struct FrameBuffer [data size disposal ts])
(define-motile-struct VideoParams [width height fpsNum fpsDen])

(define (dispose-FrameBuffer f)
  ((FrameBuffer.disposal f)))

(define (FrameBuffer->Frame v)
  (Frame (if (equal? (bytes-length (FrameBuffer.data v)) (FrameBuffer.size v))
             (FrameBuffer.data v)
             (subbytes (FrameBuffer.data v) 0 (FrameBuffer.size v)))
         (FrameBuffer.ts v)))

#|(define b (AddCURL 'foo))
b
(AddCURL? b)
(AddCURL.curl b)
(AddCURL? (AddCURL!curl b 'bar))|#