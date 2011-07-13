#! /usr/bin/env racket
#lang racket/base

(require "environs.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/net/structs.rkt"
         "../../peer/src/api/compilation.rkt"
         "../../peer/src/api/message.rkt"
         racket/match
         racket/function)

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

(define vp8-curl (synthesize-scurl-string "vp8"))
(define speex-curl (synthesize-scurl-string "speex"))
(define vorbis-curl (synthesize-scurl-string "vorbis"))

(define (reply-with-payload reply-curl payload)
  (ask/send "POST" request-thread (vector-ref reply-curl 0) (vector-ref reply-curl 1) (vector-ref reply-curl 2)
            payload
            #:url (vector-ref reply-curl 3)))

(let loop ()
  (define v (thread-receive))
  (match v
    ; spawn a new vp8 decoder
    [(vector '<tuple> '(mischief message ask) "SPAWN" "/" body '(("accepts" . "video/webm")) reply echo)
     (hash-set! curls=>threads vp8-curl (spawn (start-program body VIDEO-DECODE)))
     (printf "vp8 decoder installed at ~a~n" vp8-curl)
     (reply-with-payload reply vp8-curl)]
    
    ; spawn a new speex decoder
    [(vector '<tuple> '(mischief message ask) "SPAWN" "/" body '(("accepts" . "audio/speex")) reply echo)
     (hash-set! curls=>threads speex-curl (spawn (start-program body AUDIO-DECODE)))
     (printf "speex decoder installed at ~a~n" vp8-curl)
     (reply-with-payload reply speex-curl)]
    
    ; forward a video packet to the vp8 decoder
    [(vector '<tuple> '(mischief message ask) "POST" (? (curry equal? vp8-curl) u) body metadata reply echo)
     (thread-send (hash-ref curls=>threads vp8-curl) (start-program body VIDEO-DECODE))]
    
    ; forward an audio packet to the speex decoder
    [(vector '<tuple> '(mischief message ask) "POST" (? (curry equal? speex-curl) u) body metadata reply echo)
     (thread-send (hash-ref curls=>threads speex-curl) (start-program body AUDIO-DECODE))]
    
    [_ #f])
  (loop))