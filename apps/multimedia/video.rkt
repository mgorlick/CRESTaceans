#lang racket

(require racket/provide
         "message-types.rkt"
         "bindings/vp8/vp8.rkt")

(provide (matching-identifiers-out #rx"^vp8dec.*" (all-from-out "bindings/vp8/vp8.rkt"))
         vp8enc-delete
         vp8enc-quartersize-new
         dispose-FrameBuffer
         FrameBuffer->Frame
         (except-out (all-defined-out) vp8enc-new* vp8enc-encode* vp8enc-encode-quarter*)
         (rename-out (vp8enc-new* vp8enc-new)
                     (vp8enc-encode* vp8enc-encode)
                     (vp8enc-encode-quarter* vp8enc-encode-quarter)
                     (v4l2-reader-setup video-reader-setup)
                     (v4l2-reader-is-ready video-reader-is-ready?)
                     (v4l2-reader-delete video-reader-delete)))

(define (vp8enc-new* params)
  (vp8enc-new (:VideoParams/width params)
              (:VideoParams/height params)
              (:VideoParams/fpsNum params)
              (:VideoParams/fpsDen params)))

(define (vp8enc-quartersize-new params)
  (vp8enc-new (/ (:VideoParams/width params) 2)
              (/ (:VideoParams/height params) 2)
              (:VideoParams/fpsNum params)
              (:VideoParams/fpsDen params)))

(define (vp8enc-encode* e frame outbuff)
  (define written (vp8enc-encode e (:FrameBuffer/size frame)
                                 (:FrameBuffer/data frame)
                                 (bytes-length outbuff)
                                 outbuff))
  (FrameBuffer/new (subbytes outbuff 0 written) written #f (:FrameBuffer/ts frame)))

(define (vp8enc-encode-quarter* e frame outbuff row col)
  (define written (vp8enc-encode-quarter e row col
                                         (:FrameBuffer/size frame)
                                         (:FrameBuffer/data frame)
                                         (bytes-length outbuff)
                                         outbuff))
  (FrameBuffer/new (subbytes outbuff 0 written) written #f (:FrameBuffer/ts frame)))

(define (video-reader-get-params v)
  (define-values (w h num den buffct) (v4l2-reader-get-params v))
  (VideoParams/new w h num den))

(define (video-reader-get-frame v ts)
  (define-values (data framenum size index) (v4l2-reader-get-frame v))
  (FrameBuffer/new data size (λ () (v4l2-reader-enqueue-buffer v index)) ts))

(define (dispose-FrameBuffer f)
  ((:FrameBuffer/disposal f)))

(define (FrameBuffer->Frame v)
  (Frame/new (if (equal? (bytes-length (:FrameBuffer/data v)) (:FrameBuffer/size v))
                 (:FrameBuffer/data v)
                 (subbytes (:FrameBuffer/data v) 0 (:FrameBuffer/size v)))
             (:FrameBuffer/ts v)))