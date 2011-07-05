#! /usr/bin/env racket
#lang racket/base

(require "../../peer/src/multimed/vp8/vp8dec.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/net/structs.rkt"
         "../../peer/src/api/compilation.rkt"
         "../../peer/src/api/message.rkt"
         racket/match)

(define *RHOST* *LOCALHOST*)
(define *RPORT* 5000)

(define *LOCALPORT* 1235)

(define me (current-thread))

(define curls=>threads (make-hash))

(define k (generate-key/defaults))
(define this-scurl (generate-scurl/defaults *LOCALHOST* *LOCALPORT* #:key k))
(define request-thread (run-tcp-peer *LOCALHOST* *LOCALPORT* this-scurl me))

(printf "Listening on ~a~n" (regexp-split "/" (scurl->string this-scurl)))
(define decoder0 (thread (make-vp8-decoder me)))

(let loop ()
  (match (thread-receive)
    [(vector '<tuple> '(mischief message ask) #"POST" url body metadata reply-curl echo)
     (if (bytes? body)
         (thread-send decoder0 body)
         (printf "body is not bytes: ~a~n" body))])
  (loop))