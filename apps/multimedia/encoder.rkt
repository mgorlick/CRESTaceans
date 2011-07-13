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

(define me (current-thread))

(define *RKEY* (with-handlers ([exn:fail? (λ (e) (printf "NO KEY SPECIFIED!~n") #f)])
                 (string->bytes/utf-8 (vector-ref (current-command-line-arguments) 0))))
(define *LOCALPORT* (with-handlers ([exn:fail? (λ (e) 5000)])
                      (string->number (vector-ref (current-command-line-arguments) 1))))

(define *LISTENING-ON* *LOCALHOST*)
(define *RHOST* *LOCALHOST*)
(define *RPORT* 1235)

(define k (generate-key/defaults))
(define this-scurl (generate-scurl/defaults *LISTENING-ON* *LOCALPORT* #:key k))
(define the-key-in-this-scurl (get-public-key this-scurl))
(define request-thread (run-tcp-peer *LISTENING-ON* *LOCALPORT* this-scurl (current-thread)))

(define make-curl (curry message/uri/new (get-public-key this-scurl) (cons *LISTENING-ON* *LOCALPORT*)))

(define (do-spawn spawn metadata rpy-to)
  ;; spawn the spawn, get the contact uri back
  (ask/send "SPAWN" request-thread *RHOST* *RPORT* *RKEY* spawn
            #:url (message/uri/new (bytes->string/utf-8 *RKEY*) (cons *RHOST* *RPORT*) "/") ;; entry point
            #:metadata metadata
            #:reply (make-curl rpy-to))
  (define targeturl (start-program (:message/ask/body (thread-receive))))
  targeturl)

(define (relayer targeturl)
  (λ ()
    (let loop ()
      (match (thread-receive)
        [(FrameBuffer buffer len disp ts)
         (ask/send "POST" request-thread *RHOST* *RPORT* *RKEY* `(FrameBuffer ,(subbytes buffer 0 len) ,len #f ,ts)
                   #:url targeturl)
         (disp)
         (loop)]
        [(? bytes? buffer)
         (ask/send "POST" request-thread *RHOST* *RPORT* *RKEY* `(FrameBuffer ,buffer ,(bytes-length buffer) #f 0)
                   #:url targeturl)
         (loop)]))))

(cond [(equal? *LOCALPORT* 5000)
       (define targeturl (do-spawn video-decoder '(("accepts" . "video/webm")) "/relay"))
       (define relay (thread (relayer targeturl)))
       (define vp8 (thread (make-vp8-encoder me relay)))
       (define video (thread (make-v4l2-reader me vp8)))
       (no-return)]
      
      [else
       (define enc (new-speex-encoder 3))
       (define targeturl (do-spawn (speex-decoder (vector-ref enc 1)) '(("accepts" . "audio/speex")) "/relay"))
       (define relay (thread (relayer targeturl)))
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
       (no-return)])