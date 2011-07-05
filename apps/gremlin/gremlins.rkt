#! /usr/bin/env racket
#lang racket/base

(require "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/api/compilation.rkt"
         net/url)

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

(define the-gremlin 
  `(lambda (t)
     (letrec ([end (lambda ()
                     (printf "Gremlin number ~a dies~n" t)
                     t)]
              [loop (lambda () 
                      (printf "Gremlin number ~a eats a pie~n" t)
                      (sleep 1)
                      (if (equal? t (random (+ t 1)))
                          (end)
                          (loop)))])
       (loop))))

(for ([i (in-range 10)])
  (ask/send "SPAWN" request-thread *RHOST* *RPORT* *RKEY* the-gremlin
            #:echo (string->immutable-string the-key-in-this-scurl))
  (sleep 1))
(no-return)