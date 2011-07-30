#lang racket/base

(require "message-types.rkt"
         "bindings/vp8/vp8.rkt"
         "bindings/speex/speex.rkt"
         "../../peer/src/api/compilation.rkt"
         (for-syntax racket/base))
(provide (all-defined-out))

(define (flip f)
  (λ (b a)
    (f a b)))

(define-syntax (global-defines stx)
  (syntax-case stx ()
    [(k p ...)
     #'(list (case (procedure-arity p)
               [(0) (define/global/0 'p p)]
               [(1) (define/global/1 'p p)]
               [(2) (define/global/2 'p p)]
               [(3) (define/global/3 'p p)]
               [else (define/global/N 'p p)])
             ...
             )]))

(define (++ base-environment . global-defines-lists)
  (foldl (flip pairs/environ) base-environment global-defines-lists))

(define UTIL
  (++ ENVIRON/TEST
      (global-defines message/ask? message/ask/new :message/ask/method :message/ask/url
                      :message/ask/body :message/ask/metadata :message/ask/reply :message/ask/echo)
      (global-defines message/tell? message/tell/new :message/tell/status :message/tell/reason
                      :message/tell/body :message/tell/metadata :message/tell/echo)
      (global-defines message/uri? message/uri/new :message/uri/scheme
                      :message/uri/authority :message/uri/path :message/uri/query)
      (global-defines void printf thread-receive
                      current-inexact-milliseconds
                      exact->inexact)
      (global-defines bytes? byte? bytes make-bytes bytes-ref bytes-length 
                      bytes-copy subbytes bytes-append
                      bytes=? bytes<? bytes>?
                      bitwise-and bitwise-ior bitwise-xor bitwise-not
                      bitwise-bit-set? bitwise-bit-field arithmetic-shift integer-length)))

(define MULTIMEDIA-BASE
  (++ UTIL
      (global-defines
       AddCURL AddCURL? AddCURL.curl AddCURL!curl
       RemoveCURL RemoveCURL? RemoveCURL.curl RemoveCURL!curl
       None None? Quit Quit?
       Frame Frame? Frame.data Frame.timestamp Frame!data Frame!timestamp
       FrameBuffer FrameBuffer? FrameBuffer.size FrameBuffer.data FrameBuffer.ts dispose-FrameBuffer
       VideoParams VideoParams? VideoParams.width VideoParams.height VideoParams.fpsNum VideoParams.fpsDen
       FrameBuffer->Frame)))

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
       (define/global/N 'vp8enc-encode/return-frame
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
  (++ MULTIMEDIA-BASE (global-defines vp8dec-new vp8dec-delete vp8dec-decode)))

(define AUDIO-ENCODE
  (++ MULTIMEDIA-BASE (global-defines new-speex-encoder speex-encoder-encode delete-speex-encoder)))

(define AUDIO-DECODE
  (++ MULTIMEDIA-BASE (global-defines new-speex-decoder speex-decoder-decode delete-speex-decoder)))