#! /usr/bin/env racket
#lang racket/base

(require "pipeline/vp8dec.rkt"
         "pipeline/vorbisdec.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/net/structs.rkt"
         "../../peer/src/api/compilation.rkt"
         "../../peer/src/api/message.rkt"
         racket/match)

(define (synthesize-scurl-string path)
  (scurl->string (scurl->public-scurl (generate-scurl/defaults *LOCALHOST* *LOCALPORT* #:key k #:path path))))

(define *RHOST* *LOCALHOST*)
(define *RPORT* 5000)

(define *LOCALPORT* 1235)

(define me (current-thread))

(define curls=>threads (make-hash))

(define k (generate-key/defaults))
(define this-scurl (generate-scurl/defaults *LOCALHOST* port #:key k))
(define request-thread (run-tcp-peer *LOCALHOST* port this-scurl processor))

(printf "Listening on ~a~n" (regexp-split "/" (scurl->string this-scurl)))

(define video0-curl (synthesize-scurl-string "video0"))
(hash-set! curls=>threads video0-curl (thread (make-vp8-decoder me)))
(define audio0-curl (synthesize-scurl-string "audio0"))
(hash-set! curls=>threads audio0-curl (thread (make-vorbis-decoder me)))
(printf "video0 decoder installed at ~a~n" video0-curl)
(printf "audio0 decoder installed at ~a~n" audio0-curl)

(let loop ()
  (define v (thread-receive))
  (match v
    [(vector '<tuple> '(mischief message ask) "POST" "/video0" body metadata reply-curl echo)
     (thread-send (hash-ref curls=>threads video0-curl) (vector-ref body 1))]
    [(vector '<tuple> '(mischief message ask) "POST" "/audio0" body metadata reply-curl echo)
     (thread-send (hash-ref curls=>threads audio0-curl) (vector-ref body 1))])
  (loop))