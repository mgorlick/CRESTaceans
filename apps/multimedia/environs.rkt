#lang racket/base

(require "message-types.rkt"
         "bindings/vp8/vp8.rkt"
         "bindings/speex/speex.rkt"
         "../../peer/src/api/compilation.rkt"
         (for-syntax racket/base))
(provide (all-defined-out))

(define-syntax ++
  (syntax-rules ()
    [(++ b1 b2)
     (pairs/environ b1 b2)]
    [(++ b1 b2 b3 ...)
     (++ (++ b1 b2) b3 ...)]))

(define UTIL
  (++ ENVIRON/TEST
      (list (define/global/0 'void void)
            (define/global/N 'printf printf)
            (define/global/0 'thread-receive thread-receive)
            (define/global/N 'thread-send thread-send)
            (define/global/0 'current-inexact-milliseconds current-inexact-milliseconds)
            (define/global/1 'exact->inexact exact->inexact)
            (define/global/1 'bytes? bytes?)
            (define/global/N 'make-bytes make-bytes)
            (define/global/2 'bytes-ref bytes-ref)
            (define/global/1 'bytes-length bytes-length)
            (define/global/1 'bytes-copy bytes-copy)
            (define/global/N 'subbytes subbytes)
            (define/global/N 'bitwise-and bitwise-and)
            (define/global/0 'None None)
            (define/global/1 'None? None?)
            (define/global/0 'Quit Quit)
            (define/global/1 'Quit? Quit?))))

(define MULTIMEDIA-BASE
  (++ UTIL
      (list (define/global/N 'FrameBuffer FrameBuffer)
            (define/global/1 'FrameBuffer? FrameBuffer?)
            (define/global/1 'FrameBuffer-size FrameBuffer-size)
            (define/global/1 'FrameBuffer-data FrameBuffer-data)
            (define/global/1 'FrameBuffer-ts FrameBuffer-ts)
            (define/global/1 'FrameBuffer-age (λ (v) (- (current-inexact-milliseconds) (FrameBuffer-ts v))))
            (define/global/1 'dispose-FrameBuffer dispose-FrameBuffer)
            (define/global/N 'VideoParams VideoParams)
            (define/global/1 'VideoParams? VideoParams?)
            (define/global/1 'VideoParams-width VideoParams-width)
            (define/global/1 'VideoParams-height VideoParams-height)
            (define/global/1 'VideoParams-fpsNum VideoParams-fpsNum)
            (define/global/1 'VideoParams-fpsDen VideoParams-fpsDen))))

(define VIDEO-ENCODE
  (++ MULTIMEDIA-BASE
      (list (define/global/N 'vp8enc-new vp8enc-new)
            (define/global/1 'vp8enc-delete vp8enc-delete)
            (define/global/N 'vp8enc-encode/return-frame
              (λ (e frame outbuff)
                (define written (vp8enc-encode e (FrameBuffer-size frame) (FrameBuffer-data frame)
                                               (bytes-length outbuff) outbuff))
                (dispose-FrameBuffer frame)
                (FrameBuffer (subbytes outbuff 0 written) written #f (FrameBuffer-ts frame))))
            (define/global/0 'video-reader-setup v4l2-reader-setup)
            (define/global/1 'video-reader-get-params
              (λ (v)
                (define-values (w h num den buffct) (v4l2-reader-get-params v))
                (VideoParams w h num den)))
            (define/global/1 'video-reader-is-ready? v4l2-reader-is-ready)
            (define/global/2 'video-reader-get-frame
              (λ (v ts)
                (define-values (data framenum size index) (v4l2-reader-get-frame v))
                (FrameBuffer data size (λ () (v4l2-reader-enqueue-buffer v index)) ts))))))

(define VIDEO-DECODE
  (++ MULTIMEDIA-BASE
      (list (define/global/0 'vp8dec-new vp8dec-new)
            (define/global/1 'vp8dec-delete vp8dec-delete)
            (define/global/N 'vp8dec-decode vp8dec-decode))))

(define AUDIO-DECODE
  (++ MULTIMEDIA-BASE
      (list (define/global/1 'new-speex-encoder new-speex-encoder)
            (define/global/1 'delete-speex-encoder delete-speex-encoder)
            (define/global/1 'new-speex-decoder new-speex-decoder)
            (define/global/1 'delete-speex-decoder delete-speex-decoder)
            (define/global/2 'speex-encoder-encode speex-encoder-encode)
            (define/global/3 'speex-decoder-decode speex-decoder-decode))))