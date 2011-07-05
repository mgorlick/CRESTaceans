#! /usr/bin/env racket
#lang racket/base

(require "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/api/compilation.rkt"
         racket/match)

(define SCHOOLBUS
  (pairs/environ
   ENVIRON/TEST
   (list (define/global/1 'thread thread)
         (define/global/0 'thread-receive thread-receive)
         (define/global/N 'printf printf)
         (define/global/N 'random random))))

(define *RHOST* *LOCALHOST*)
(define *RPORT* 5000)

(define *LOCALPORT* 1235)

(define curls=>threads (make-hash))

(define k (generate-key/defaults))
(define this-scurl (generate-scurl/defaults *LOCALHOST* *LOCALPORT* #:key k))
(define request-thread (run-tcp-peer *LOCALHOST* *LOCALPORT* this-scurl (current-thread)))

(printf "Listening on ~a~n" (regexp-split "/" (scurl->string this-scurl)))

(define (handle-message message t)
  (match message
    [(vector '<tuple> '(mischief message ask) "SPAWN" "/" body metadata reply-curl echo)
     
     ;; set up the program itself plus the CURL naming it
     (define new-gremlin (thread (λ () (start-program body SCHOOLBUS t))))
     (define new-gremlin-curl
       (scurl->string
        (scurl->public-scurl
         (generate-scurl/defaults *LOCALHOST* *LOCALPORT* #:key k #:path (format "gremlin~a" t)))))
     
     ;; store the CURL
     (hash-set! curls=>threads new-gremlin-curl new-gremlin)
     (printf "Installing ~a at ~s~n" new-gremlin new-gremlin-curl)
     
     ;; reply back to the sender to deliver the program's new CURL 
     (ask/send "POST" request-thread *RHOST* *RPORT* echo ; echo-blob hardcoded to contain remote key
               (string->immutable-string new-gremlin-curl))]
    
    [any-other-pattern (printf "some other message: ~s~n" any-other-pattern)]))

(let loop ([t 0])
  (handle-message (thread-receive) t)
  (loop (add1 t)))