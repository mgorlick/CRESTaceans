#! /usr/bin/env racket
#lang racket/base

(require "environs.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/net/structs.rkt"
         "../../peer/src/api/compilation.rkt"
         "../../peer/src/api/message.rkt"
         racket/match)

(define (synthesize-scurl-string path)
  (scurl->string (scurl->public-scurl (generate-scurl/defaults *LISTENING-ON* *LOCALPORT* #:key k #:path path))))

(define-syntax spawn
  (syntax-rules ()
    [(_ body ...)
     (thread (λ () body ...))]))

(define *RHOST* "128.195.58.146")
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

(let loop ()
  (define v (thread-receive))
  (match v
    ; spawn a new video decoder
    [(vector '<tuple> '(mischief message ask) "SPAWN" "/" body '(("accepts" . "video/webm")) reply-curl echo)
     (hash-set! curls=>threads video0-curl (spawn (start-program body VIDEO-DECODE)))
     (printf "video0 decoder installed at ~a~n" video0-curl)]
    
    ; spawn a new audio decoder
    [(vector '<tuple> '(mischief message ask) "SPAWN" "/" body '(("accepts" . "audio/webm")) reply-curl echo)
     (hash-set! curls=>threads audio0-curl (spawn (start-program body AUDIO-DECODE)))
     (printf "audio0 decoder installed at ~a~n" audio0-curl)]
    
    ; forward a video packet to the video decoder
    [(vector '<tuple> '(mischief message ask) "POST" "/video0" body metadata reply-curl echo)
     (thread-send (hash-ref curls=>threads video0-curl) (start-program body VIDEO-DECODE))]
    
    ; forward an audio packet to the audio decoder
    [(vector '<tuple> '(mischief message ask) "POST" "/audio0" body metadata reply-curl echo)
     (thread-send (hash-ref curls=>threads audio0-curl) (start-program body AUDIO-DECODE))])
  (loop))