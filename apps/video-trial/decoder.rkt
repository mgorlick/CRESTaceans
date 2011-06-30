#! /usr/bin/env racket
#lang racket/base

(require "../../peer/src/multimed/vp8/vp8dec.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/net/structs.rkt"
         "../../peer/src/api/compilation.rkt"
         "../../peer/src/api/message.rkt"
         racket/match)

(define me (current-thread))
(define this-scurl (generate-scurl/defaults *LOCALHOST* 5000))
(define server (run-tcp-peer *LOCALHOST* 5000 this-scurl me))
(printf "Listening on ~a~n" (scurl->string this-scurl))

(define decoder0 (thread (make-vp8-decoder me)))

(let loop ()
  (match (thread-receive)
    [(vector '<tuple> '(mischief message ask) #"RAW" url (? procedure? body) a b c)
     (let ([val (mischief/start body)])
       (when (bytes? val)
         (thread-send decoder0 val)))])
  (loop))