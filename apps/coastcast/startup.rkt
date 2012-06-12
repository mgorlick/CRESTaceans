#! /usr/bin/env racket
#lang racket/base

(require "motiles.rkt"
         "envs.rkt"
         "message-types.rkt"
         "config.rkt"
         "../../ui/ws.rkt"
         "../../peer/src/net/tcp-peer.rkt"         
         "../../Motile/persistent/hash.rkt"
         "../../Motile/compile/serialize.rkt"
         "../../Motile/generate/baseline.rkt"
         "../../Motile/actor/actor.rkt"
         "../../Motile/actor/curl.rkt"
         (prefix-in base: "../../Motile/actor/send.rkt")
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
         racket/dict
         racket/async-channel)

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
(base:set-inter-island-router! COMM-thd)

(define-syntax-rule (define-wrapper-for f g precall)
  (define (g . xs)
    (apply precall xs)
    (apply f xs)))

(define-syntax-rule (define-wrapper-for* f g postcall)
  (define (g . xs)
    (apply f xs)
    (apply postcall xs)))

(define (my-root-loop ui ui-notification-channel)
  (define tre (thread-receive-evt))
  (define e (sync tre ui-notification-channel))
  (displayln "Synced")
  (define amsg (cond [(equal? tre e) (thread-receive)]
                     [else e]))
  ;(define amsg (thread-receive))
  (match amsg
    ;; spawn format, to be solely directed at the public curl.
    [(vector (? (curry equal? PUBLIC-CURL) pcurl) (match:spawn body metadata reply))
     (define the-nickname (gensym (or (metadata-ref metadata "nick") 'nonamegiven)))
     (define-values (actor actor/loc) (actor/new ROOT the-nickname))
     ; make a chart to track the encoder send counts.
     (define this-chart        
       (if (eq? (metadata-ref metadata "nick") 'encoder)
           (new-chart 'line (format "sending count of ~a" the-nickname) "total messages")
           #f))
     (when this-chart (ui-send! ui this-chart))
     ; a producer of functions that count and send a data point every 10 invocations.
     (define (make-counter)
       (let ([count 0])
         (lambda _
           (set! count (add1 count))
           (when (and this-chart (zero? (modulo count 10)))
             (ui-send! ui (plot-a-point this-chart (current-inexact-milliseconds) count))))))
     ; make a counter for curl/send.
     (define counter (make-counter))
     (define-wrapper-for base:curl/send curl/send counter)
     (define benv (++ (metadata->benv metadata)
                      (global-value-defines the-nickname PUBLIC-CURL)
                      (global-defines this/locative
                                      curl/get-public
                                      curl/send)))
     ; todo: use `curl/send` instead of `curl/send*` (fix name shadowing problem in macro definition)
     (actor/jumpstart actor 
                      (λ ()
                        (printf "Actor starting: ~s~n" the-nickname)
                        (let ([ret (motile/call body benv)])
                          (printf "Actor ending: ~s~n" the-nickname)
                          ret)))]
    ;; a delivery from off-island into one of the actors here.
    [(? delivery? m)
     ; send count deprecation happens here.
     (with-handlers ([exn? (λ (e) (displayln e))])
       (base:curl/forward m))]
    [else
     (displayln "Root: discarding a message:")
     (displayln amsg)])
  (my-root-loop ui ui-notification-channel))


;;; start the root chieftain up.
(actor/jumpstart ROOT (λ ()                        
                        (define (open-a-tab/synch)
                          (define ac (make-async-channel))
                          (define s (open-a-tab (λ (json) (async-channel-put ac json))))
                          (values s ac))
                        (define-values (s ui-notification-channel) (open-a-tab/synch)) 
                        (ui-wait-for-readiness s)
                        (PROMISSARY ROOT)
                        (my-root-loop s ui-notification-channel)))

(unless (argsassoc "--no-gui")
  (base:curl/send PUBLIC-CURL (spawn/new gui-controller (make-metadata is/gui (nick 'gui-controller)) #f)))
(unless (argsassoc "--no-video")
  (define device (argsassoc "--device" #:default "/dev/video0" #:no-val "/dev/video0"))
  (define w (argsassoc "--w" #:default 640 #:no-val 640 #:call (compose round string->number)))
  (define h (argsassoc "--h" #:default 480 #:no-val 480 #:call (compose round string->number)))
  (define encoder-where@ (curl/get-public (argsassoc "--vhost" #:no-val *LISTENING-ON* #:default *LISTENING-ON*)
                                          (argsassoc "--vport" #:call string->number 
                                                     #:no-val *LOCALPORT*
                                                     #:default *LOCALPORT*)))
  (define pubsub-where@ (if (equal? "dsite" (argsassoc "--psat")) PUBLIC-CURL encoder-where@))
  (define the-bang (motile/call make-big-bang
                                MULTIMEDIA-BASE
                                encoder-where@ device w h
                                pubsub-where@
                                PUBLIC-CURL))
  (base:curl/send PUBLIC-CURL (spawn/new the-bang (make-metadata (nick 'big-bang)) #f))
  (displayln "Spawn sent"))

(semaphore-wait (make-semaphore))