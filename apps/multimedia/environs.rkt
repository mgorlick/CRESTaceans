#lang racket/base

(require "message-types.rkt"
         "gui.rkt"
         "bindings/vp8/vp8.rkt"
         "bindings/speex/speex.rkt"
         "../../peer/src/api/message.rkt"
         "../../peer/src/api/compilation.rkt"
         racket/function
         (for-syntax racket/base))
(provide (all-defined-out))

(define (message/uri->string u)
  (format "[imp: ~a ~a ~a]" (:message/uri/authority u) (:message/uri/path u) (:message/uri/query u)))

(define UTIL
  (++ BASELINE
      (require-spec->global-defines (except-in "../../peer/src/api/message.rkt" ask tell uri))
      (global-defines message/uri->string
                      void printf thread-receive sleep display vector-ref
                      current-inexact-milliseconds
                      exact->inexact)
      (global-defines bytes? byte? bytes make-bytes bytes-ref bytes-length 
                      bytes-copy subbytes bytes-append
                      bytes=? bytes<? bytes>?
                      bitwise-and bitwise-ior bitwise-xor bitwise-not
                      bitwise-bit-set? bitwise-bit-field arithmetic-shift integer-length)))

(define MULTIMEDIA-BASE
  (++ UTIL (require-spec->global-defines "message-types.rkt")))

(define VIDEO-ENCODE
  (++ MULTIMEDIA-BASE
      (list
       (define/global/1 'vp8enc-new 
         (λ (params)
           (vp8enc-new (VideoParams.width params)
                       (VideoParams.height params)
                       (VideoParams.fpsNum params)
                       (VideoParams.fpsDen params))))
       (define/global/1 'vp8enc-delete vp8enc-delete)
       (define/global/3 'vp8enc-encode/return-frame
         (λ (e frame outbuff)
           (define written (vp8enc-encode e (FrameBuffer.size frame)
                                          (FrameBuffer.data frame)
                                          (bytes-length outbuff)
                                          outbuff))
           (dispose-FrameBuffer frame)
           (FrameBuffer (subbytes outbuff 0 written) written #f (FrameBuffer.ts frame)))))
      
      (list
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
      (global-defines vp8dec-new vp8dec-delete vp8dec-decode vp8dec-decode-copy)
      (require-spec->global-defines "gui.rkt")))

(define AUDIO-ENCODE
  (++ MULTIMEDIA-BASE (global-defines new-speex-encoder speex-encoder-encode delete-speex-encoder)))

(define AUDIO-DECODE
  (++ MULTIMEDIA-BASE (global-defines new-speex-decoder speex-decoder-decode delete-speex-decoder)))