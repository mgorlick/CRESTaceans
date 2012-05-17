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

; current-gui-curl: enforces the constraint that a gui is a unique global resource on an island.
(define current-gui-curl
  (let ([c #f]
        [writelock (make-semaphore 1)]
        [readlock (make-semaphore 0)])
    (case-lambda
      [() (call-with-semaphore readlock (λ () 
                                          c))]
      [(f) (call-with-semaphore writelock (λ ()
                                            (set! c f)
                                            (semaphore-post readlock)))])))

; allow an actor to get the current gui curl, set it, neither, or both, depending on binding environment.
; `current-gui-curl' itself shouldn't be added to a binding environment.
(define get-current-gui-curl (procedure-reduce-arity current-gui-curl 0))
(define set-current-gui-curl! (procedure-reduce-arity current-gui-curl 1))

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
(define-syntax-rule (metadata-entry key val)
  `(,(symbol->string 'key) . ,val))
(define-syntax-rule (define-metadata-entry-maker n)
  (define (n v)
    (metadata-entry n v)))
(define-metadata-entry-maker nick)
(define-metadata-entry-maker from)

(define (eval-definition e)
  (motile/call e BASELINE))
(define-syntax global-motile-point-of-definition-evals
  (syntax-rules ()
    [(k id ...)
     `((id . ,(eval-definition id))
       ...)]))

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
                      delivery/promise-fulfillment
                      big-bang
                      encoder-side-relay
                      forward-relay
                      video-reader/encoder
                      video-decoder/single
                      gui-controller)
      (global-motile-point-of-definition-evals canvas-endpoint 
                                               linker-bang
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
      (global-defines get-current-gui-curl set-current-gui-curl!)
      (require-spec->global-defines "gui.rkt")))
(define GUI-ENDPOINT
  (++ MULTIMEDIA-BASE
      (require-spec->global-defines "gui.rkt")))