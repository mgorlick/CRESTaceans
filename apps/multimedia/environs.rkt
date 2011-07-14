#lang racket/base

(require "bindings/vp8/vp8.rkt"
         "bindings/speex/speex.rkt"
         "bindings/vorbis/libvorbis.rkt"
         "pipeline/structs.rkt"
         "../../peer/src/api/compilation.rkt")
(provide (all-defined-out))

(define-syntax ++
  (syntax-rules ()
    [(++ b1 b2)
     (pairs/environ b1 b2)]
    [(++ b1 b2 b3 ...)
     (++ (pairs/environ b1 b2) b3 ...)]))

(define MULTIMEDIA-BASE
  (++ ENVIRON/TEST
      (list (define/global/0 'void void)
            (define/global/N 'printf printf)
            (define/global/0 'thread-receive thread-receive)
            (define/global/N 'thread-send thread-send)
            (define/global/0 'current-inexact-milliseconds current-inexact-milliseconds)
            (define/global/1 'exact->inexact exact->inexact)
            (define/global/2 'bytes-ref bytes-ref)
            (define/global/1 'bytes-length bytes-length)
            (define/global/1 'bytes-copy bytes-copy)
            (define/global/N 'subbytes subbytes)
            (define/global/N 'bitwise-and bitwise-and)
            (define/global/N 'FrameBuffer FrameBuffer)
            (define/global/1 'FrameBuffer? FrameBuffer?)
            (define/global/1 'FrameBuffer-size FrameBuffer-size)
            (define/global/1 'FrameBuffer-data FrameBuffer-data)
            (define/global/1 'FrameBuffer-ts FrameBuffer-ts)
            (define/global/1 'FrameBuffer-age (λ (v) (- (current-inexact-milliseconds) (FrameBuffer-ts v))))
            (define/global/1 'dispose-FrameBuffer dispose-FrameBuffer)
            (define/global/0 'Quit Quit)
            (define/global/1 'Quit? Quit?))))

(define AUDIO-DECODE
  (++ MULTIMEDIA-BASE
      (list (define/global/1 'new-speex-encoder new-speex-encoder)
            (define/global/1 'delete-speex-encoder delete-speex-encoder)
            (define/global/1 'new-speex-decoder new-speex-decoder)
            (define/global/1 'delete-speex-decoder delete-speex-decoder)
            (define/global/2 'speex-encoder-encode speex-encoder-encode)
            (define/global/3 'speex-decoder-decode speex-decoder-decode)
            (define/global/0 'vorbisdec-new vorbisdec-new)
            (define/global/1 'vorbisdec-is-init vorbisdec-is-init)
            (define/global/3 'header-packet-in header-packet-in)
            (define/global/3 'data-packet-blockin data-packet-blockin))))

(define VIDEO-DECODE
  (++ MULTIMEDIA-BASE
      (list (define/global/0 'vp8dec-new vp8dec-new)
            (define/global/1 'vp8dec-delete vp8dec-delete)
            (define/global/N 'vp8dec-decode vp8dec-decode))))