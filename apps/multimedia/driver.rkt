#! /usr/bin/env racket
#lang racket/base

(require "clan.rkt"
         "motiles.rkt"
         "misc.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/api/compilation.rkt")

(define accepts/webm '(("accepts" . "video/webm")))
(define produces/webm '(("produces" . "video/webm")))
(define type/webm '(("content-type" . "video/webm")))

(define accepts/speex '(("accepts" . "audio/speex")))
(define produces/speex '(("produces" . "audio/speex")))
(define type/speex '(("content-type" . "audio/speex")))

(define *RHOST* (assoc/or/default "--rhost" *LOCALHOST*))
(define *RPORT* (assoc/or/default "--rport" 1235 #:call string->number))
(define *RKEY* #f)

(cond [(assoc/or/default "--video")
       (define relay-curl (make-curl (uuid)))
       (handle-spawn relay-curl (motile/compile (relayer type/webm)) type/webm root-curl)
       
       (handle-spawn (make-curl (uuid)) (motile/compile video-reader/encoder) produces/webm relay-curl)
       
       ;(ask/send request-thread "SPAWN" (remote-curl-root *RKEY* *RHOST* *RPORT*)
        ;          video-reader/encoder #:metadata produces/webm #:reply relay-curl)
       
       (ask/send request-thread "SPAWN" (remote-curl-root *RKEY* *RHOST* *RPORT*)
                 video-decoder #:metadata accepts/webm #:reply relay-curl)])

(cond [(assoc/or/default "--audio")
       (define relay-curl (make-curl (uuid)))
       (handle-spawn relay-curl (motile/compile (relayer type/speex))
                     type/speex root-curl)
       
       (handle-spawn (make-curl (uuid)) (motile/compile (audio-reader/speex-encoder 3 relay-curl))
                     produces/speex root-curl)
       
       (ask/send request-thread "SPAWN" (remote-curl-root *RKEY* *RHOST* *RPORT*)
                 (speex-decoder 640) #:metadata accepts/speex #:reply relay-curl)])

(interpreter)