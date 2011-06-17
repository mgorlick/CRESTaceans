#! /usr/bin/env racket
#lang racket/base

(require "../../peer/src/multimed/vp8/vp8dec.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/net/structs.rkt"
         "../../peer/src/api/compilation.rkt"
         "../../peer/src/api/message.rkt"
         racket/match)

(define me (current-thread))
(define server (run-tcp-peer "128.195.58.146" 5000 me))

(define decoder0 (make-vp8-decoder me))

(let loop ()
  (let ([req (deserialize/recompile (response-data (thread-receive)))])
    (thread-send decoder0 (:message/ask/body req)))
  (loop))