#! /usr/bin/env racket
#lang racket/base

(require "../src/net/listener.rkt"
         "../../old/Mischief/baseline.rkt"
         "../../old/Mischief/z.rkt"
         "../../old/Mischief/xserialize.rkt"
         racket/match
         racket/async-channel)

(define (deserialize/recompile bstr [be BASELINE])
  (deserialize (read (open-input-bytes bstr)) be #f))

;; enet stuff

(define reply-channel (make-async-channel))
(define request-channel (run-listener "localhost" 1234 reply-channel))

(define (cc) (call/cc call/cc))

(define t 0)

(let loop ()
  (match (async-channel-get reply-channel)
    [(response host port data)
     (thread (λ () ((mischief/start (deserialize/recompile data)) (cc) t)))
     (set! t (add1 t))
     (loop)]))