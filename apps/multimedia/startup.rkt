#! /usr/bin/env racket
#lang racket/base

(require "motiles.rkt"
         "envs.rkt"
         "message-types.rkt"
         "config.rkt"
         "../../peer/src/net/tcp-peer.rkt"         
         "../../Motile/persistent/hash.rkt"
         "../../Motile/baseline.rkt"
         "../../Motile/compile/compile.rkt"
         "../../Motile/compile/serialize.rkt"
         "../../Motile/generate/baseline.rkt"
         "../../Motile/actor/actor.rkt"
         "../../Motile/actor/curl.rkt"
         "../../Motile/actor/jumpstart.rkt"
         "../../Motile/actor/island.rkt"
         "../../Motile/actor/locative.rkt"
         "../../Motile/actor/logger.rkt"
         racket/function
         racket/list
         racket/match
         racket/contract
         racket/dict)

; argsassoc: string [call: string -> any] [no-val: any] [default: any] -> any
; separates the provided command line arguments of the form:
;             key1=val1 key2=val2 key3
; if the provided key name is present and has a value, returns `call' applied to that value.
; if the provided key name is present but has no value, returns `default'.
; if the provided key name is not present, returns `no-val'.
(define CLargs (map (curry regexp-split #rx"=") (vector->list (current-command-line-arguments))))
(define (argsassoc key 
                   #:call [call values] 
                   #:no-val [no-val #f] 
                   #:default [default #t])
  (let ([entry (assoc key CLargs)])
    (if entry
        (if (> (length entry) 1)
            (call (second entry))
            default)
        no-val)))

(define (meta-has-any? meta . vs)
  (ormap (λ (k.v)
           (if (equal? (cdr k.v) (hash/ref meta (car k.v) #f))
               (begin
                 (printf "~s == ~s~n" (cdr k.v) (hash/ref meta (car k.v) 'nothere))
                 #t)
               (begin
                 (printf "~s != ~s~n" (cdr k.v) (hash/ref meta (car k.v) 'nothere))
                 #f)))
         vs))

;; host and port to listen on. use to start the comm layer below, designating root to receive incoming.
(define *LISTENING-ON* (argsassoc "--host" #:no-val *LOCALHOST*))
(define *LOCALPORT* (argsassoc "--port" #:no-val 5000 #:call string->number))

(define ADDRESS-HERE (island/address/new 
                      #"abcdefghijklmnopqrstuvwxyz" 
                      (string->bytes/utf-8 *LISTENING-ON*) *LOCALPORT*))

(this/island ADDRESS-HERE)
(printf "Island starting: ~s~n" (this/island))

; make root actor
(define-values (ROOT ROOT/LOCATIVE) (actor/root/new))
; deliver incoming messages to ROOT
(define COMM-thd (run-tcp-peer *LISTENING-ON* *LOCALPORT* (actor/thread ROOT) #:encrypt? #f))
(set-box! inter-island-router COMM-thd)

(define (metadata->benv metadata)
  (cond
    [(meta-has-any? metadata accepts/webm) VIDEO-DECODE]
    [(meta-has-any? metadata produces/webm) VIDEO-ENCODE]
    [(meta-has-any? metadata is/gui) GUI]
    [(meta-has-any? metadata is/endpoint) GUI-ENDPOINT]
    [else MULTIMEDIA-BASE]))

(define/contract (curl/get-public host port)
  ((or/c string? bytes?) exact-nonnegative-integer? . -> . curl?)
  (motile/deserialize (dict-ref PUBLICS (cons (if (string? host) 
                                                  (string->bytes/utf-8 host)
                                                  host)
                                              port) #f) #f))

;; sneaky: derive a new locative from the root locative (a "public locative"),
;; then serialize some curl derived from it in order to stick the public locative
;; into the exports table
(define PUBLIC/LOCATIVE 
  (locative/cons/any ROOT/LOCATIVE
                     A-LONG-TIME
                     A-LONG-TIME
                     #t #t))
(locative/id! PUBLIC/LOCATIVE 'public)
(define ____ (motile/serialize (curl/new/any PUBLIC/LOCATIVE '() #f)))
;; end sneakiness.

(define PUBLIC/CURL (curl/get-public *LISTENING-ON* *LOCALPORT*))

(define (my-root-loop)
  (define amsg (thread-receive))
  (match amsg
    [(cons (? (curry equal? PUBLIC/CURL) pcurl)
           (match:spawn body metadata reply))
     (define-values (actor actor/loc)
       (actor/new ROOT (gensym (or (metadata-ref metadata 'nick)
                                   'nonamegiven))))
     (actor/jumpstart actor 
                      (λ ()
                        (motile/call body (++ (metadata->benv metadata)
                                              (global-value-defines PUBLIC/CURL)
                                              (global-defines this/locative
                                                              this/island
                                                              curl/get-public
                                                              motile/serialize)))))]
    [(cons c@ (match:remote body metadata reply))
     ;; send count deprecation happens here.
     (with-handlers ([exn? (λ (e) (displayln e))])
       (curl/send c@ (cdr amsg)))]
    [else
     (displayln "Root: discarding a message")])
  (my-root-loop))

;;; start the root chieftain up.
(actor/jumpstart ROOT my-root-loop)

(unless (argsassoc "--no-gui")
  (define the-controller (gui-controller))
  (curl/send PUBLIC/CURL (spawn/new the-controller (make-metadata is/gui '(nick . gui-controller)) #f)))
(unless (argsassoc "--no-video")
  (define the-bang (big-bang PUBLIC/CURL "/dev/video0" 640 480 PUBLIC/CURL))
  (curl/send PUBLIC/CURL (spawn/new the-bang (make-metadata '(nick . big-bang)) #f)))

(semaphore-wait (make-semaphore))