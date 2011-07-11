#! /usr/bin/env racket
#lang racket/base

(require "bindings/vp8/vp8.rkt"
         "pipeline/vorbisdec.rkt"
         "pipeline/structs.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/net/structs.rkt"
         "../../peer/src/api/compilation.rkt"
         "../../peer/src/api/message.rkt"
         racket/match)

(define (synthesize-scurl-string path)
  (scurl->string (scurl->public-scurl (generate-scurl/defaults *LISTENING-ON* *LOCALPORT* #:key k #:path path))))

(define VIDEO
  (pairs/environ
   ENVIRON/TEST
   (list (define/global/0 'thread-receive thread-receive)
         (define/global/0 'current-inexact-milliseconds current-inexact-milliseconds)
         (define/global/1 'exact->inexact exact->inexact)
         (define/global/N 'printf printf)
         (define/global/0 'vp8dec-new vp8dec-new)
         (define/global/1 'vp8dec-delete vp8dec-delete)
         (define/global/N 'vp8dec-decode vp8dec-decode)
         (define/global/1 'FrameBuffer-size FrameBuffer-size)
         (define/global/1 'FrameBuffer-data FrameBuffer-data)
         (define/global/1 'FrameBuffer-ts FrameBuffer-ts)
         (define/global/1 'dispose-FrameBuffer dispose-FrameBuffer))))

(define *RHOST* *LOCALHOST*)
(define *RPORT* 5000)
(define *LISTENING-ON* *LOCALHOST*)
(define *LOCALPORT* 1235)

(define me (current-thread))

(define curls=>threads (make-hash))

(define k (generate-key/defaults))
(define this-scurl (generate-scurl/defaults *LISTENING-ON* *LOCALPORT* #:key k))
(define request-thread (run-tcp-peer *LISTENING-ON* *LOCALPORT* this-scurl me))

(printf "Listening on ~a~n" (regexp-split "/" (scurl->string this-scurl)))

(define video0-curl (synthesize-scurl-string "video0"))
(define audio0-curl (synthesize-scurl-string "audio0"))
(hash-set! curls=>threads audio0-curl (thread (make-vorbis-decoder me)))
(printf "audio0 decoder installed at ~a~n" audio0-curl)

(define (ts-vector->FB body)
  (make-FrameBuffer (vector-ref body 1) (bytes-length (vector-ref body 1)) void (vector-ref body 0)))

(let loop ()
  (define v (thread-receive))
  (match v
    ; spawn a new video decoder
    [(vector '<tuple> '(mischief message ask) "SPAWN" "/" body metadata reply-curl echo)
     (printf "~a~n" VIDEO)
     (define new (thread (λ () (start-program body VIDEO))))
     (hash-set! curls=>threads video0-curl new)
     (printf "video0 decoder installed at ~a~n" video0-curl)]
    
    ; forward a video packet to the video decoder
    [(vector '<tuple> '(mischief message ask) "POST" "/video0" body metadata reply-curl echo)
     (thread-send (hash-ref curls=>threads video0-curl) (ts-vector->FB body))]
    
    ; forward an audio packet to the audio decoder
    [(vector '<tuple> '(mischief message ask) "POST" "/audio0" body metadata reply-curl echo)
     (thread-send (hash-ref curls=>threads audio0-curl) (ts-vector->FB body))])
  (loop))