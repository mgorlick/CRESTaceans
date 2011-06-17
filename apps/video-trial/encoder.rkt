#! /usr/bin/env racket
#lang racket/base

(require "../../peer/src/multimed/vp8/v4l2-reader.rkt"
         "../../peer/src/multimed/vp8/vp8enc.rkt"
         "../../peer/src/multimed/structs.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/net/structs.rkt"
         "../../peer/src/api/compilation.rkt"
         racket/match)

(define me (current-thread))
(define server (run-tcp-peer "128.195.59.191" 1234 me))

(define *RHOST* "128.195.58.146")
(define *RPORT* 5000)

(define urls (make-hash))

(define encoder0 (thread (make-vp8-encoder me me)))
(define video0 (thread (make-v4l2-reader me encoder0)))

(let loop ()
  (match (thread-receive)
    [(FrameBuffer buffer len λdisp)
     (define frame (subbytes buffer 0 len))
     (λdisp)
     (compile/serialize #"RAW" server *RHOST* *RPORT* (subbytes buffer 0 len))
     (loop)]))