#lang racket

(require "message-types.rkt"
         "bindings/vp8/vp8.rkt")

(provide (except-out (all-defined-out) vp8enc-new*)
         (rename-out (vp8enc-new* vp8enc-new))
         vp8enc-delete
         vp8dec-new
         vp8dec-delete
         vp8dec-decode-copy
         vp8dec-decode-pip)

(define (vp8enc-new* params)
  (vp8enc-new (VideoParams.width params)
              (VideoParams.height params)
              (VideoParams.fpsNum params)
              (VideoParams.fpsDen params)))

(define (vp8enc-encode/return-frame e frame outbuff)
  (define written (vp8enc-encode e (FrameBuffer.size frame)
                                 (FrameBuffer.data frame)
                                 (bytes-length outbuff)
                                 outbuff))
  (dispose-FrameBuffer frame)
  (FrameBuffer (subbytes outbuff 0 written) written #f (FrameBuffer.ts frame)))

(define (video-reader-get-params v)
  (define-values (w h num den buffct) (v4l2-reader-get-params v))
  (VideoParams w h num den))
(define video-reader-setup v4l2-reader-setup)
(define video-reader-is-ready? v4l2-reader-is-ready)
(define (video-reader-get-frame v ts)
  (define-values (data framenum size index) (v4l2-reader-get-frame v))
  (FrameBuffer data size (λ () (v4l2-reader-enqueue-buffer v index)) ts))