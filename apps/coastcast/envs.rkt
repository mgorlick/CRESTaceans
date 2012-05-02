#lang racket/base

(require "gui.rkt"
         "video.rkt"
         "message-types.rkt"
         "motiles.rkt"
         "../../Motile/persistent/environ.rkt"
         "../../Motile/persistent/hash.rkt"
         "../../Motile/baseline.rkt"
         "../../Motile/compile/compile.rkt"
         "../../Motile/generate/baseline.rkt"
         "../../Motile/actor/island.rkt"
         "../../Motile/actor/curl.rkt"
         "../../Motile/actor/locative.rkt"
         "../../Motile/actor/promise.rkt"
         "../../Motile/actor/delivery.rkt"
         "../../Motile/actor/send.rkt"
         racket/require
         racket/function
         racket/list
         (for-syntax racket/base)
         (planet "uuid-v4.rkt" ("zitterbewegung" "uuid-v4.plt" 2 0)))

(provide (all-defined-out))


;; extra stuff to put in baseline. this should eventually be moved to Motile/baseline.rkt
(define-syntax-rule (global-value-defines id ...)
  `((id . ,id) ...))
(define bin- (procedure-reduce-arity - 2))
(define bin+ (procedure-reduce-arity + 2))
(define bin* (procedure-reduce-arity * 2))
(define bin/ (procedure-reduce-arity / 2))
(define min* (procedure-reduce-arity min 2))
(define max* (procedure-reduce-arity max 2))
(define bin>= (procedure-reduce-arity >= 2))
(define sleep* (procedure-reduce-arity sleep 1))
(define (halve x) (/ x 2))
(define mailbox-get-message thread-receive)
(define (mailbox-has-message? [n 0])
  (and (sync/timeout n (thread-receive-evt)) #t))

(define A-LONG-TIME 2.76e110)

(define current-gui-curl #f)
(define readlock (make-semaphore 0))
(define writelock (make-semaphore 1))
(define (get-current-gui-curl)
  (call-with-semaphore readlock (λ () 
                                  (displayln "Current GUI curl discovered") 
                                  current-gui-curl)))
(define (set-current-gui-curl! c)
  (printf "Current GUI curl set~n")
  (set! current-gui-curl c)
  (semaphore-post readlock))
(define (reset-current-gui-curl!)
  (printf "Current GUI curl unset~n")
  (set! current-gui-curl #f)
  (set! readlock (make-semaphore 0)))
(define (with-gui-singleton-actor thunk)
  (call-with-semaphore writelock thunk))
;; std metadata types used to determine call convention and binding environment allocation
(define accepts/webm '("accepts" . "video/webm"))
(define produces/webm '("produces" . "video/webm"))
(define type/webm '("content-type" . "video/webm"))
(define is/gui '("is" . "gui"))
(define is/endpoint '("is" . "endpoint"))
(define is/proxy '("is" . "proxy"))

(define (make-metadata . vals)
  (pairs/hash hash/equal/null vals))
(define (metadata-ref m k)
  (hash/ref m k #f))
(define-syntax-rule (define-metadata-entry-maker n)
  (define (n v)
    `(,(symbol->string 'n) . ,v)))
(define-metadata-entry-maker nick)
(define-metadata-entry-maker from)

(define-syntax global-motile-points-of-definition
  (syntax-rules ()
    [(k id ...)
     `((id . ,id) ...)]))

;; binding environments used.
(define MULTIMEDIA-BASE
  (++ BASELINE
      (global-defines bin* bin- bin+ bin/ bin>= min* sleep* max* halve
                      display displayln void printf vector-ref
                      mailbox-get-message mailbox-has-message?
                      current-inexact-milliseconds exact->inexact)
      (global-defines bytes? byte? bytes make-bytes bytes-ref bytes-length 
                      bytes-copy subbytes bytes-append
                      bytes=? bytes<? bytes>?
                      bitwise-and bitwise-ior bitwise-xor bitwise-not
                      bitwise-bit-set? bitwise-bit-field arithmetic-shift integer-length)
      (require-spec->global-defines racket/list)
      (require-spec->global-defines (matching-identifiers-in #rx"^(?!(match:)).*$" "message-types.rkt"))
      (global-defines make-metadata
                      metadata-ref
                      nick
                      from
                      A-LONG-TIME
                      sleep*
                      make-uuid
                      island/address/get-dns
                      island/address/get-port
                      island/address/get-public
                      curl?
                      curl/ok?
                      curl/intra?
                      curl/new
                      curl/new/any
                      curl/send
                      curl/send/multiple
                      curl/send/promise
                      curl/pretty
                      curl/get-meta
                      curl/get-path
                      curl/get-island-address
                      curl/target=?
                      locative?
                      locative/revoked?
                      locative/cons
                      locative/cons/any
                      locative/send
                      locative/pretty
                      promise?
                      promise/kept?
                      promise/ruined?
                      promise/wait
                      promise/new
                      promise/to-fulfill
                      promise/result
                      delivery?
                      delivery/contents-sent
                      delivery/curl-used
                      delivery/promise-fulfillment)
      (global-motile-points-of-definition gui-controller
                                          make-video-reader/encoder
                                          make-big-bang
                                          make-encoder-side-relay
                                          make-forward-relay
                                          canvas-endpoint 
                                          linker-bang
                                          make-video-decoder/single
                                          make-video-decoder/pip)
      (global-value-defines accepts/webm 
                            produces/webm 
                            type/webm
                            is/gui 
                            is/proxy 
                            is/endpoint)))

(define VIDEO-ENCODE
  (++ MULTIMEDIA-BASE
      (require-spec->global-defines (matching-identifiers-in #rx"^video-reader.*" "video.rkt"))
      (require-spec->global-defines (matching-identifiers-in #rx"^vp8enc.*" "video.rkt"))
      (global-defines dispose-FrameBuffer FrameBuffer->Frame)))
(define VIDEO-DECODE
  (++ MULTIMEDIA-BASE
      (require-spec->global-defines (matching-identifiers-in #rx"^vp8dec.*" "video.rkt"))
      (global-defines get-current-gui-curl)))
(define GUI
  (++ MULTIMEDIA-BASE
      (global-defines set-current-gui-curl! reset-current-gui-curl!)
      (require-spec->global-defines "gui.rkt")))
(define GUI-ENDPOINT
  (++ MULTIMEDIA-BASE
      (global-defines vp8dec-new yuv420p-to-rgb32 greyscale)
      (require-spec->global-defines (matching-identifiers-in #rx"^color-converter.*" "video.rkt"))
      (require-spec->global-defines "gui.rkt")))