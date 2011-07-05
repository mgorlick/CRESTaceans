#! /usr/bin/env racket
#lang racket/base

(require "../../peer/src/multimed/vp8/v4l2-reader.rkt"
         "../../peer/src/multimed/vp8/vp8enc.rkt"
         "../../peer/src/multimed/structs.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/net/structs.rkt"
         "../../peer/src/api/compilation.rkt"
         racket/match)

(require profile)

(define *RKEY*
  (with-handlers ([exn:fail? (λ (e) (printf "NO KEY SPECIFIED!~n") #f)])
    (string->bytes/utf-8 (vector-ref (current-command-line-arguments) 0))))

;(profile-thunk
; (λ ()
(define me (current-thread))
(define this-scurl (generate-scurl/defaults *LOCALHOST* 1234))
(define server (run-tcp-peer *LOCALHOST* 1234 this-scurl me))

(define *RHOST* *LOCALHOST*)
(define *RPORT* 5000)

(define urls (make-hash))

(define encoder0 (thread (make-vp8-encoder me me)))
(define video0 (thread (make-v4l2-reader me encoder0)))

(let loop ()
  (match (thread-receive)
    [(FrameBuffer buffer len λdisp)
     (define frame (subbytes buffer 0 len))
     (λdisp)
     (ask/send #"RAW" server *RHOST* *RPORT* *RKEY* (subbytes buffer 0 len))
     (loop)]))
;   )
; #:threads #t)