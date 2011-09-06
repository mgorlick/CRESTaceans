#lang racket

(require racket/provide
         "message-types.rkt"
         "bindings/vp8/vp8.rkt")

(provide (matching-identifiers-out #rx"^vp8dec.*" (all-from-out "bindings/vp8/vp8.rkt"))
         vp8enc-delete
         dispose-FrameBuffer
         (except-out (all-defined-out) vp8enc-new* vp8enc-encode* vp8enc-encode-quarter*)
         (rename-out (vp8enc-new* vp8enc-new)
                     (vp8enc-encode* vp8enc-encode)
                     (vp8enc-encode-quarter* vp8enc-encode-quarter)
                     (v4l2-reader-setup video-reader-setup)
                     (v4l2-reader-is-ready video-reader-is-ready?)
                     (v4l2-reader-delete video-reader-delete)))

(define (vp8enc-new* params)
  (vp8enc-new (VideoParams.width params)
              (VideoParams.height params)
              (VideoParams.fpsNum params)
              (VideoParams.fpsDen params)))

(define (vp8enc-encode* e frame outbuff)
  (define written (vp8enc-encode e (FrameBuffer.size frame)
                                 (FrameBuffer.data frame)
                                 (bytes-length outbuff)
                                 outbuff))
  (FrameBuffer (subbytes outbuff 0 written) written #f (FrameBuffer.ts frame)))

(define (vp8enc-encode-quarter* e frame outbuff row col)
  (define-values (x y)
    (match* (row col)
      [('top 'left) (values 0 0)]
      [('top 'right) (values 0 1)]
      [('bottom 'left) (values 1 0)]
      [('bottom 'right) (values 1 1)]))
  (define written (vp8enc-encode-quarter e x y
                                         (FrameBuffer.size frame)
                                         (FrameBuffer.data frame)
                                         (bytes-length outbuff)
                                         outbuff))
  (FrameBuffer (subbytes outbuff 0 written) written #f (FrameBuffer.ts frame)))

(define (video-reader-get-params v)
  (define-values (w h num den buffct) (v4l2-reader-get-params v))
  (VideoParams w h num den))

(define (video-reader-get-frame v ts)
  (define-values (data framenum size index) (v4l2-reader-get-frame v))
  (FrameBuffer data size (λ () (v4l2-reader-enqueue-buffer v index)) ts))