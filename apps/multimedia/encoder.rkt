#! /usr/bin/env racket
#lang racket/base

(require "motiles.rkt" "misc.rkt" "environs.rkt"
         "bindings/speex/speex.rkt"
         "pipeline/v4l2-reader.rkt"
         "pipeline/vp8enc.rkt"
         "pipeline/structs.rkt"
         "pipeline/bufferpool.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/api/compilation.rkt"
         racket/match
         racket/function)

(define *RKEY* (with-handlers ([exn:fail? (λ (e) (printf "NO KEY SPECIFIED!~n") #f)])
                 (string->bytes/utf-8 (vector-ref (current-command-line-arguments) 0))))
(define *LOCALPORT* (with-handlers ([exn:fail? (λ (e) 5000)])
                      (string->number (vector-ref (current-command-line-arguments) 1))))

(define *LISTENING-ON* "128.195.59.199")
(define *RHOST* "128.195.59.199")
(define *RPORT* 1235)

(define k (generate-key/defaults))
(define this-scurl (generate-scurl/defaults *LISTENING-ON* *LOCALPORT* #:key k))
(define request-thread (run-tcp-peer *LISTENING-ON* *LOCALPORT* this-scurl (current-thread)))

(define make-curl (curry message/uri/new #f (cons *LISTENING-ON* *LOCALPORT*)))
(define root-curl (make-curl "/"))
(define relay-curl (make-curl (uuid)))

(define (remote-curl-root rkey rhost rport)
  (message/uri/new rkey (cons rhost rport) "/"))

(define (relayer targeturl)
  (match (thread-receive)
    [(? message/uri? new)
     (ask/send request-thread "POST" targeturl `(Quit))
     (printf "Swapping target to ~a~n" new)
     (relayer new)]
    [(FrameBuffer buffer len disp ts)
     (ask/send request-thread "POST" targeturl `(FrameBuffer ,(subbytes buffer 0 len) ,len #f ,ts))
     (disp)
     (relayer targeturl)]))

(define (handler relay)
  (define v (start-program (:message/ask/body (thread-receive))))
  (when (message/uri? v)
    (thread-send relay v))
  (handler relay))

(cond [(equal? *LOCALPORT* 5000)
       (do-spawn request-thread video-decoder '(("accepts" . "video/webm"))
                 (remote-curl-root *RKEY* *RHOST* *RPORT*)
                 relay-curl)
       (define targeturl (start-program (:message/ask/body (thread-receive))))
       (define relay (spawn (relayer targeturl)))
       
       (define vp8 (thread (make-vp8-encoder (current-thread) relay)))
       (define video (thread (make-v4l2-reader (current-thread) vp8)))
       (handler relay)]
      
      [else
       (define enc (new-speex-encoder 3))
       (do-spawn request-thread (speex-decoder (vector-ref enc 1)) '(("accepts" . "audio/speex"))
                 (remote-curl-root *RKEY* *RHOST* *RPORT*)
                 relay-curl)
       (define targeturl (start-program (:message/ask/body (thread-receive))))
       (define relay (spawn (relayer targeturl)))
       
       (define speexenc
         (thread
          (λ ()
            (define-values (handler request) (make-bufferpool-handler 50 1000))
            (let loop ()
              (define ts (current-inexact-milliseconds))
              (define-values (buff return) (request))
              (define available (speex-encoder-encode (vector-ref enc 0) buff))
              (thread-send relay (FrameBuffer buff available return ts))
              (loop)))))
       (handler relay)])