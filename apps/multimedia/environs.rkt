#lang racket/base

(require "message-types.rkt"
         "gui.rkt"
         "video.rkt"
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


(define accepts/webm '("accepts" . "video/webm"))
(define produces/webm '("produces" . "video/webm"))
(define type/webm '("content-type" . "video/webm"))

(define accepts/speex '("accepts" . "audio/speex"))
(define produces/speex '("produces" . "audio/speex"))
(define type/speex '("content-type" . "audio/speex"))

(define (metadata . x)
  x)

(define MULTIMEDIA-BASE
  (++ UTIL
      (require-spec->global-defines "message-types.rkt")
      (global-defines metadata)
      `((accepts/webm . ,accepts/webm)
        (produces/webm . ,produces/webm)
        (type/webm . ,type/webm)
        (accepts/speex . ,accepts/speex)
        (produces/speex . ,produces/speex)
        (type/speex . ,type/speex))))

(define VIDEO-ENCODE
  (++ MULTIMEDIA-BASE
      (require-spec->global-defines 
       (only-in "video.rkt"
                vp8enc-new
                vp8enc-delete
                vp8enc-encode/return-frame
                video-reader-setup
                video-reader-get-params
                video-reader-is-ready?
                video-reader-get-frame))))

(define VIDEO-DECODE
  (++ MULTIMEDIA-BASE
      (require-spec->global-defines (only-in "video.rkt" 
                                             vp8dec-new
                                             vp8dec-delete 
                                             vp8dec-decode-copy))
      (require-spec->global-defines "gui.rkt")))

(define AUDIO-ENCODE
  (++ MULTIMEDIA-BASE (global-defines new-speex-encoder speex-encoder-encode delete-speex-encoder)))

(define AUDIO-DECODE
  (++ MULTIMEDIA-BASE (global-defines new-speex-decoder speex-decoder-decode delete-speex-decoder)))