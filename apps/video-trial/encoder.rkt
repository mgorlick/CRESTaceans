#! /usr/bin/env racket
#lang racket/base

(require "../../peer/src/multimed/vp8/v4l2-reader.rkt"
         "../../peer/src/multimed/vp8/vp8enc.rkt"
         "../../peer/src/multimed/structs.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/net/structs.rkt"
         "../../peer/src/api/compilation.rkt"
         racket/match)

(define me (current-thread))

(define *RKEY*
  (with-handlers ([exn:fail? (λ (e) (printf "NO KEY SPECIFIED!~n") #f)])
    (string->bytes/utf-8 (vector-ref (current-command-line-arguments) 0))))

(define port
  (with-handlers ([exn:fail? (λ (e) 5000)])
    (string->number (vector-ref (current-command-line-arguments) 1))))

(define processor
  (thread
   (λ ()
     (let loop ()
       (printf "new CURL: ~s~n" (start-program (:message/ask/body (thread-receive))))
       (loop)))))

(define k (generate-key/defaults))
(define this-scurl (generate-scurl/defaults *LOCALHOST* port #:key k))
(define request-thread (run-tcp-peer *LOCALHOST* port this-scurl processor))

(define the-key-in-this-scurl
  (path/param-path (list-ref (url-path (string->url (scurl->string this-scurl))) 1)))

(define *RHOST* *LOCALHOST*)
(define *RPORT* 1235)

(define encoder0 (thread (make-vp8-encoder me me)))
(define video0 (thread (make-v4l2-reader me encoder0)))

(let loop ()
  (match (thread-receive)
    [(FrameBuffer buffer len λdisp)
     (define frame (subbytes buffer 0 len))
     (λdisp)
     (ask/send #"POST" request-thread *RHOST* *RPORT* *RKEY* (subbytes buffer 0 len) #:compile? #f)
     (loop)]))