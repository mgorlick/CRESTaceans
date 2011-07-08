#! /usr/bin/env racket
#lang racket/base

(require "pipeline/vp8dec.rkt"
         "pipeline/vorbisdec.rkt"
         "pipeline/structs.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/net/structs.rkt"
         "../../peer/src/api/compilation.rkt"
         "../../peer/src/api/message.rkt"
         racket/match)

(define (synthesize-scurl-string path)
  (scurl->string (scurl->public-scurl (generate-scurl/defaults *LISTENING-ON* *LOCALPORT* #:key k #:path path))))

(define *RHOST* *LOCALHOST*)
(define *RPORT* 5000)

(define *LISTENING-ON* *LOCALHOST*)
(define *LOCALPORT* 1235)

(define me (current-thread))

(define curls=>threads (make-hash))

(define k (generate-key/defaults))
(define this-scurl (generate-scurl/defaults *LISTENING-ON* *LOCALPORT* #:key k))
(define request-thread (run-tcp-peer *LISTENING-ON* *LOCALPORT* this-scurl (current-thread)))

(printf "Listening on ~a~n" (regexp-split "/" (scurl->string this-scurl)))

(define video0-curl (synthesize-scurl-string "video0"))
(hash-set! curls=>threads video0-curl (thread (make-vp8-decoder me)))
(define audio0-curl (synthesize-scurl-string "audio0"))
(hash-set! curls=>threads audio0-curl (thread (make-vorbis-decoder me)))
(printf "video0 decoder installed at ~a~n" video0-curl)
(printf "audio0 decoder installed at ~a~n" audio0-curl)

(define (ts-vector->FB body)
  (make-FrameBuffer (vector-ref body 1) (bytes-length (vector-ref body 1)) void (vector-ref body 0)))

(let loop ()
  (define v (thread-receive))
  (match v
    [(vector '<tuple> '(mischief message ask) "POST" "/video0" body metadata reply-curl echo)
     (thread-send (hash-ref curls=>threads video0-curl) (ts-vector->FB body))]
    [(vector '<tuple> '(mischief message ask) "POST" "/audio0" body metadata reply-curl echo)
     (thread-send (hash-ref curls=>threads audio0-curl) (ts-vector->FB body))])
  (loop))