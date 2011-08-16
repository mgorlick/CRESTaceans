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

(define bin- (procedure-reduce-arity - 2))
(define bin+ (procedure-reduce-arity + 2))
(define bin* (procedure-reduce-arity * 2))
(define bin/ (procedure-reduce-arity / 2))
(define min* (procedure-reduce-arity min 2))
(define max* (procedure-reduce-arity max 2))
(define bin>= (procedure-reduce-arity >= 2))
(define sleep* (procedure-reduce-arity sleep 1))

(define UTIL
  (++ BASELINE
      (require-spec->global-defines (except-in "../../peer/src/api/message.rkt" ask tell uri))
      (global-defines bin* bin- bin+ bin/ bin>= min* sleep* max*
                      displayln message/uri->string void printf thread-receive display
                      vector-ref current-inexact-milliseconds exact->inexact)
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

(define is/gui '("is" . "gui"))

(define (metadata . x)
  x)

; current-gui-curl: enforces the constraint that a gui is a unique global resource on an island.
(define current-gui-curl
  (let ([c #f] [readlock (make-semaphore 0)])
    (case-lambda
      [() (call-with-semaphore readlock (λ () c))]
      [(f) (displayln "GUI CURL changed:")
           (displayln f)
           (set! c f)
           (semaphore-post readlock)])))

; allow an actor to get the current gui curl, set it, neither, or both, depending on binding environment.
; `current-gui-curl' itself shouldn't be added to a binding environment.
(define get-current-gui-curl (procedure-reduce-arity current-gui-curl 0))
(define set-current-gui-curl! (procedure-reduce-arity current-gui-curl 1))

(define MULTIMEDIA-BASE
  (++ UTIL
      (require-spec->global-defines "message-types.rkt")
      (global-defines metadata sleep*)
      `((accepts/webm . ,accepts/webm)
        (produces/webm . ,produces/webm)
        (type/webm . ,type/webm)
        (accepts/speex . ,accepts/speex)
        (produces/speex . ,produces/speex)
        (type/speex . ,type/speex)
        (is/gui . ,is/gui))))

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
      (global-defines get-current-gui-curl)))

(define GUI
  (++ MULTIMEDIA-BASE 
      (global-defines get-current-gui-curl set-current-gui-curl!)
      (require-spec->global-defines "gui.rkt")))

(define AUDIO-ENCODE
  (++ MULTIMEDIA-BASE (global-defines new-speex-encoder speex-encoder-encode delete-speex-encoder)))

(define AUDIO-DECODE
  (++ MULTIMEDIA-BASE (global-defines new-speex-decoder speex-decoder-decode delete-speex-decoder)))