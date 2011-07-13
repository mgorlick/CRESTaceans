#! /usr/bin/env racket
#lang racket/base

(require "environs.rkt" "misc.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/net/structs.rkt"
         "../../peer/src/api/compilation.rkt"
         "../../peer/src/api/message.rkt"
         racket/match
         racket/function)

(define *RHOST* "128.195.58.146")
(define *RPORT* 5000)
(define *LISTENING-ON* *LOCALHOST*)
(define *LOCALPORT* 1235)

(define me (current-thread))

(define curls=>threads (make-hash))

(define k (generate-key/defaults))
(define this-scurl (generate-scurl/defaults *LISTENING-ON* *LOCALPORT* #:key k))
(define request-thread (run-tcp-peer *LISTENING-ON* *LOCALPORT* this-scurl me))

(define make-curl 
  (curry message/uri/new
         (get-public-key this-scurl)
         (cons *LISTENING-ON* *LOCALPORT*)))

(define root-curl (make-curl "/"))
(define vp8-curl (make-curl "/vp8"))
(define speex-curl (make-curl "/speex"))

(define (reply-with-payload r payload)
  (ask/send "POST" request-thread
            (car (:message/uri/authority r))
            (cdr (:message/uri/authority r))
            (:message/uri/scheme r)
            payload
            #:url r))

(printf "listening on ~s~n" root-curl)

(let loop ()
  (define v (thread-receive))
  (match v
    ; spawn a new vp8 decoder
    [(ask "SPAWN" (? (is? root-curl) u) body '(("accepts" . "video/webm")) reply echo)
     (hash-set! curls=>threads vp8-curl (spawn (start-program body VIDEO-DECODE)))
     (printf "vp8 decoder installed at ~a~n" vp8-curl)
     (reply-with-payload reply vp8-curl)]
    
    ; spawn a new speex decoder
    [(ask "SPAWN" (? (is? root-curl) u) body '(("accepts" . "audio/speex")) reply echo)
     (hash-set! curls=>threads speex-curl (spawn (start-program body AUDIO-DECODE)))
     (printf "speex decoder installed at ~a~n" vp8-curl)
     (reply-with-payload reply speex-curl)]
    
    ; forward a video packet to the vp8 decoder
    [(ask "POST" (? (is? vp8-curl) u) body metadata reply echo)
     (thread-send (hash-ref curls=>threads vp8-curl) (start-program body VIDEO-DECODE))]
    
    ; forward an audio packet to the speex decoder
    [(ask "POST" (? (is? speex-curl) u) body metadata reply echo)
     (thread-send (hash-ref curls=>threads speex-curl) (start-program body AUDIO-DECODE))])
  (loop))