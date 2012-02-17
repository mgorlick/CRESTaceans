#! /usr/bin/env racket
#lang racket/base

(require "motiles.rkt"
         "envs.rkt"
         "message-types.rkt"
         "config.rkt"
         "../../peer/src/net/tcp-peer.rkt"         
         "../../Motile/persistent/hash.rkt"
         "../../Motile/compile/serialize.rkt"
         "../../Motile/generate/baseline.rkt"
         "../../Motile/actor/actor.rkt"
         "../../Motile/actor/curl.rkt"
         "../../Motile/actor/send.rkt"
         "../../Motile/actor/delivery.rkt"
         "../../Motile/actor/promise.rkt"
         "../../Motile/actor/jumpstart.rkt"
         "../../Motile/actor/island.rkt"
         "../../Motile/actor/locative.rkt"
         "../../Motile/actor/logger.rkt"
         racket/function
         racket/list
         racket/match
         racket/contract
         racket/dict)

(provide (all-defined-out))

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
  (ormap (λ (k.v) (equal? (cdr k.v) (hash/ref meta (car k.v) #f)))
         vs))

(define (metadata->benv metadata)
  (cond [(meta-has-any? metadata accepts/webm) VIDEO-DECODE]
        [(meta-has-any? metadata produces/webm) VIDEO-ENCODE]
        [(meta-has-any? metadata is/gui) GUI]
        [(meta-has-any? metadata is/endpoint) GUI-ENDPOINT]
        [else MULTIMEDIA-BASE]))

(define-syntax-rule (->boolean expr)
  (if expr
      #t
      #f))

(define/contract (curl/known-public? host port)
  ((or/c string? bytes?) exact-nonnegative-integer? . -> . boolean?)
  (->boolean (dict-ref PUBLICS (cons (if (string? host) (string->bytes/utf-8 host) host) port) #f)))

(define/contract (curl/get-public host port)
  ((or/c string? bytes?) exact-nonnegative-integer? . -> . curl?)
  (motile/deserialize (dict-ref PUBLICS (cons (if (string? host) (string->bytes/utf-8 host) host)
                                              port))
                      #f))

(define (make-root/get-public/register-public)
  (define-values (root root-locative) (actor/root/new))
  (define public-locative (locative/cons/any root-locative A-LONG-TIME A-LONG-TIME #t #t))
  (locative/id! public-locative '(public))
  (define public-curl (curl/new/any public-locative null #f))
  (motile/serialize public-curl) ; put in exports table.
  (values root root-locative 
          public-locative 
          (if (curl/known-public? (island/address/dns (this/island)) (island/address/port (this/island)))
              (curl/get-public (island/address/dns (this/island)) (island/address/port (this/island)))
              public-curl)
          ))

#|(for/list ([i (in-range 5000 5020)])
  (this/island (island/address/new #"abcdefghijklmnopqrstuvwxyz" #"128.195.59.227" i))
  (define-values (root rootl publicl publicc) (make-root/get-public/register-public))
 `((,(island/address/dns (this/island)) . ,i) . (,(motile/serialize (curl/new/any publicl null #f)))))|#

;; host and port to listen on. use to start the comm layer below, designating root to receive incoming.
(define *LISTENING-ON* (argsassoc "--host" #:no-val *LOCALHOST*))
(define *LOCALPORT* (argsassoc "--port" #:no-val 5000 #:call string->number))

(define ADDRESS-HERE (island/address/new 
                      #"abcdefghijklmnopqrstuvwxyz" 
                      (string->bytes/utf-8 *LISTENING-ON*) *LOCALPORT*))

(this/island ADDRESS-HERE)
(printf "Island starting: ~s~n" (this/island))
; make root actor
(define-values (ROOT ROOT-LOCATIVE PUBLIC-LOCATIVE PUBLIC-CURL) (make-root/get-public/register-public))
; deliver incoming messages to ROOT
(define COMM-thd (run-tcp-peer *LISTENING-ON* *LOCALPORT* (actor/thread ROOT) #:encrypt? #f))
(set-inter-island-router! COMM-thd)

(define (my-root-loop)
  (define amsg (thread-receive))
  (match amsg
    ;; spawn format, to be solely directed at the public curl.
    [(vector (? (curry equal? PUBLIC-CURL) pcurl) (match:spawn body metadata reply))
     (define the-nickname  (gensym (or (metadata-ref metadata "nick") 'nonamegiven)))
     (define-values (actor actor/loc) 
       (actor/new ROOT the-nickname))
     (actor/jumpstart actor 
                      (λ ()
                        (printf "Actor starting: ~s~n" the-nickname)
                        (let ([ret (motile/call body (++ (metadata->benv metadata)
                                                         (global-value-defines the-nickname PUBLIC-CURL)
                                                         (global-defines this/locative
                                                                         curl/get-public)))])
                          (printf "Actor ending: ~s~n" the-nickname)
                          ret)))]
    [(? delivery? m)
     ; send count deprecation happens here.
     (with-handlers ([exn? (λ (e) (displayln e))])
       (curl/forward m))]
    [else
     (displayln "Root: discarding a message:")
     (displayln amsg)])
  (my-root-loop))

;;; start the root chieftain up.
(actor/jumpstart ROOT (λ ()
                        (PROMISSARY ROOT)
                        (my-root-loop)))

(unless (argsassoc "--no-gui")
  (define the-controller (gui-controller))
  (curl/send PUBLIC-CURL (spawn/new the-controller (make-metadata is/gui (nick 'gui-controller)) #f)))
(unless (argsassoc "--no-video")
  (define device (argsassoc "--device" #:default "/dev/video0" #:no-val "/dev/video0"))
  (define w (argsassoc "--w" #:default 640 #:no-val 640 #:call (compose round string->number)))
  (define h (argsassoc "--h" #:default 480 #:no-val 480 #:call (compose round string->number)))
  (define encoder-where@ (curl/get-public (argsassoc "--vhost" #:no-val *LISTENING-ON* #:default *LISTENING-ON*)
                                          (argsassoc "--vport" #:call string->number 
                                                     #:no-val *LOCALPORT* 
                                                     #:default *LOCALPORT*)))
  (define pubsub-where@ (if (equal? "d" (argsassoc "--p")) PUBLIC-CURL encoder-where@))
  (define the-bang (big-bang encoder-where@ device w h
                             pubsub-where@
                             PUBLIC-CURL))
  (curl/send PUBLIC-CURL (spawn/new the-bang (make-metadata (nick 'big-bang)) #f)))

(semaphore-wait (make-semaphore))