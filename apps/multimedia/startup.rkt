#lang racket/base

(require "motiles.rkt"
         "envs.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/api/tuple-type.rkt"
         "../../Motile/baseline.rkt"
         "../../Motile/compile/compile.rkt"
         "../../Motile/compile/serialize.rkt"
         "../../Motile/generate/baseline.rkt"
         "../../Motile/actor/actor.rkt"
         "../../Motile/actor/curl.rkt"
         "../../Motile/actor/root.rkt"
         "../../Motile/actor/jumpstart.rkt"
         "../../Motile/actor/island.rkt"
         "../../Motile/actor/locative.rkt"
         racket/function
         racket/list
         racket/match)

; argsassoc: string [call: string -> any] [no-val: any] [default: any] -> any
; separates the provided command line arguments of the form:
;             --key1=val1 --key2=val2 --key3
; if the provided key name is present and has a value, returns `call' applied to that value.
; if the provided key name is present but has no value, returns `default'.
; if the provided key name is not present, returns `no-val'.
(define CLargs (map (curry regexp-split #rx"=") (vector->list (current-command-line-arguments))))
(define (argsassoc key 
                   #:call [call (λ (x) x)] 
                   #:no-val [no-val #f] 
                   #:default [default #t])
  (let ([entry (assoc key CLargs)])
    (if entry
        (if (> (length entry) 1)
            (call (second entry))
            default)
        no-val)))

(define (contains-any? mlist . vs)
  (and (list? mlist)
       (ormap (curryr member mlist) vs)))

(define-tuple-type (spawn) body metadata reply)
(define-tuple-type (remote) body metadata reply)

;; host and port to listen on. use to start the comm layer below, designating root to receive incoming.
(define *LISTENING-ON* (argsassoc "--host" #:no-val *LOCALHOST*))
(define *LOCALPORT* (argsassoc "--port" #:no-val 5000 #:call string->number))
(define COMM-thd (run-tcp-peer *LISTENING-ON* *LOCALPORT* (actor/thread ROOT)))
(set-box! inter-island-router COMM-thd)

(parameterize ([this/island (island/address/new 
                             #"abcdefghijklmnopqrstuvwxyz" 
                             (string->bytes/utf-8 *LISTENING-ON*) *LOCALPORT*)])
  (define (metadata->benv metadata)
    (cond
      [(contains-any? metadata accepts/webm) VIDEO-DECODE]
      [(contains-any? metadata produces/webm) VIDEO-ENCODE]
      [(contains-any? metadata is/gui) GUI]
      [(contains-any? metadata is/endpoint) GUI-ENDPOINT]
      [else MULTIMEDIA-BASE]))
  
  (define (my-root-loop)
    (define amsg (thread-receive))
    (match amsg
      [(cons loc (spawn body metadata reply))
       (define-values (actor actor/loc) (actor/new ROOT (gensym 'actor)))
       (displayln "Got a spawn")
       (actor/jumpstart actor (λ ()
                                (displayln "Inside jumpstart thunk")
                                (motile/call (unwrap body) (metadata->benv metadata))))]
      [(cons loc (remote body metadata reply))
       (locative/send loc body)])
    (my-root-loop))
  
  ;; sneaky: derive a new locative from the root locative (a "public locative"),
  ;; then serialize a CURL made from it (a "public curl").
  ;; why? so that others can forge up a similar CURL and use it to bootstrap their way
  ;; to messaging this island for the first time.
  (define PUBLIC/LOCATIVE 
    (locative/cons/any ROOT/LOCATIVE 
                       (+ (current-inexact-milliseconds)
                          (* 2.76 (expt 10 110))) ; will last beyond heat death of universe
                       (* 365.25 24 60 60 1000000) ; 1 message/ns for a year
                       #t #t))
  (locative/id! PUBLIC/LOCATIVE 'public)
  (define PUBLIC/CURL (motile/deserialize
                       (motile/serialize (curl/new/any PUBLIC/LOCATIVE '() #f)) #f))
  
  ;;; jumpstart the root.
  (actor/jumpstart ROOT my-root-loop)
  
  (curl/send PUBLIC/CURL
                 (spawn/new (motile/compile 
                             '(lambda ()
                                (displayln "Hello!")
                                (let loop ()
                                  (displayln (thread-receive))
                                  (loop))))
                            (make-metadata)
                            #f)))