#! /usr/bin/env racket
#lang racket/base

(require "clan.rkt"
         "motiles.rkt"
         "misc.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/api/compilation.rkt")

(define *RHOST* (assoc/or/default "--rhost" *LOCALHOST*))
(define *RPORT* (assoc/or/default "--rport" 1235 #:call string->number))
(define *RKEY* #f)

(cond [(assoc/or/default "--video" #f)
       (define relay-curl (make-curl (uuid)))
       (handle-spawn relay-curl
                     (motile/compile (relayer '(("content-type" . "video/webm"))))
                     (metadata->benv '(("content-type" . "video/webm")))
                     '(("content-type" . "video/webm"))
                     root-curl)
       
       (handle-spawn (make-curl (uuid))
                     (motile/compile (video-reader/encoder (hash-ref curls=>threads relay-curl)))
                     (metadata->benv '(("produces" . "video/webm")))
                     '(("produces" . "video/webm"))
                     root-curl)
       
       (ask/send request-thread "SPAWN" (remote-curl-root *RKEY* *RHOST* *RPORT*) 
                 video-decoder
                 #:metadata '(("accepts" . "video/webm"))
                 #:reply relay-curl)])

(cond [(assoc/or/default "--audio" #f)
       (define relay-curl (make-curl (uuid)))
       (handle-spawn relay-curl
                     (motile/compile (relayer '(("content-type" . "audio/speex"))))
                     (metadata->benv '(("content-type" . "audio/speex")))
                     '(("content-type" . "audio/speex"))
                     root-curl)
       
       (handle-spawn (make-curl (uuid))
                     (motile/compile (audio-reader/speex-encoder 3 relay-curl))
                     (metadata->benv '(("produces" . "audio/speex")))
                     '(("produces" . "audio/speex"))
                     root-curl)
       
       (ask/send request-thread "SPAWN" (remote-curl-root *RKEY* *RHOST* *RPORT*)
                 (speex-decoder 640)
                 #:metadata '(("accepts" . "audio/speex"))
                 #:reply relay-curl)])

(interpreter)