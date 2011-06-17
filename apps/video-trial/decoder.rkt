#! /usr/bin/env racket
#lang racket/base

(require "../../peer/src/multimed/vp8/vp8dec.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/net/structs.rkt"
         "../../peer/src/api/compilation.rkt"
         "../../peer/src/api/message.rkt"
         racket/match)

(define me (current-thread))
(define server (run-tcp-peer *LOCALHOST* 5000 me))

(define decoder0 (thread (make-vp8-decoder me)))

(let loop ()
  (let* ([ser (read (open-input-bytes (response-data (thread-receive))))]
         [msg (deserialize ser BASELINE #f)])
    ;(printf "~a~n" ser)
    (match msg
      [(vector '<tuple> '(mischief message ask) #"RAW" url (? procedure? body) a b c)
       (let ([val (mischief/start body)])
         (when (bytes? val)
           (thread-send decoder0 val)))]))
  (loop))