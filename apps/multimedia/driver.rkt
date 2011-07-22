#! /usr/bin/env racket
#lang racket/base

(require "clan.rkt"
         "motiles.rkt"
         "misc.rkt"
         "environs.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/api/compilation.rkt"
         racket/match
         racket/function
         unstable/function)

(define *RKEY* #f)
(define *RPORT* 1235)
(define *RHOST* *LISTENING-ON*)

(cond [(member "--video" (vector->list (current-command-line-arguments)))
       (define relay-curl (make-curl (uuid)))
       (handle-spawn relay-curl
                     (motile/compile (relayer '(("content-type" . "video/webm"))))
                     (metadata->benv '(("content-type" . "video/webm")))
                     '(("content-type" . "video/webm"))
                     root-curl)
       
       (handle-spawn (make-curl (uuid))
                     (motile/compile (video-reader/encoder relay-curl))
                     (metadata->benv '(("produces" . "video/webm")))
                     '(("produces" . "video/webm"))
                     root-curl)
       
       (ask/send request-thread "SPAWN" (remote-curl-root *RKEY* *RHOST* *RPORT*) 
                 video-decoder
                 #:metadata '(("accepts" . "video/webm"))
                 #:reply relay-curl)]
      
      [(member "--audio" (vector->list (current-command-line-arguments)))
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