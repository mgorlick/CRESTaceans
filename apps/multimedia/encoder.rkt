#! /usr/bin/env racket
#lang racket/base

(require "motiles.rkt" "misc.rkt" "message-types.rkt" "environs.rkt"
         "bindings/speex/speex.rkt"
         "pipeline/bufferpool.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/api/compilation.rkt"
         racket/function)

(define *LOCALPORT* (with-handlers ([exn:fail? (λ (e) 5000)])
                      (string->number (vector-ref (current-command-line-arguments) 0))))
(define *RKEY* (with-handlers ([exn:fail? (λ (e) (printf "NO KEY SPECIFIED!~n") #f)])
                 (string->bytes/utf-8 (vector-ref (current-command-line-arguments) 1))))

(define *LISTENING-ON* *LOCALHOST*)
(define *RHOST* *LOCALHOST*)
(define *RPORT* 1235)

(define k (generate-key/defaults))
(define this-scurl (generate-scurl/defaults *LISTENING-ON* *LOCALPORT* #:key k))
(define request-thread (run-tcp-peer *LISTENING-ON* *LOCALPORT* this-scurl (current-thread)))

(define make-curl (curry message/uri/new #f (cons *LISTENING-ON* *LOCALPORT*)))
(define root-curl (make-curl "/"))
(define relay-curl (make-curl (uuid)))
(define (remote-curl-root rkey rhost rport)
  (message/uri/new rkey (cons rhost rport) "/"))

;; primitive for sending from the Motile level
(define ask/send*
  (case-lambda
    [(method url body) (ask/send request-thread method url body)]
    [(method url body metadata) (ask/send request-thread method url body #:metadata metadata)]))
(define MULTIMEDIA-BASE* (++ MULTIMEDIA-BASE (list (define/global/N 'ask/send* ask/send*))))
(define VIDEO-ENCODE* (++ VIDEO-ENCODE (list (define/global/N 'ask/send* ask/send*))))
(define AUDIO-ENCODE* (++ AUDIO-ENCODE (list (define/global/N 'ask/send* ask/send*))))

(define (handler relay)
  (define v (motile/start (:message/ask/body (thread-receive)) MULTIMEDIA-BASE))
  (thread-send relay v)
  (handler relay))

(define-values (relay encoder)
  (cond [(equal? *LOCALPORT* 5000)
         (ask/send request-thread "SPAWN" (remote-curl-root *RKEY* *RHOST* *RPORT*) 
                   video-decoder
                   #:metadata '(("accepts" . "video/webm"))
                   #:reply relay-curl)
         
         (define relay (spawn (motile/start (motile/compile (relayer '(("content-type" . "video/webm"))))
                                            MULTIMEDIA-BASE*)))
         (define encoder (spawn (motile/start (motile/compile (video-reader/encoder relay)) VIDEO-ENCODE*)))
         (values relay encoder)]
        
        [else
         (ask/send request-thread "SPAWN" (remote-curl-root *RKEY* *RHOST* *RPORT*) 
                   (speex-decoder 640)
                   #:metadata '(("accepts" . "video/webm"))
                   #:reply relay-curl)
         
         (define relay (spawn (motile/start (motile/compile (relayer '(("content-type" . "audio/speex")))) 
                                            MULTIMEDIA-BASE*)))
         (define encoder (spawn (motile/start (motile/compile (audio-reader/encoder 3 relay)) AUDIO-ENCODE*)))
         (values relay encoder)]))

(handler relay)