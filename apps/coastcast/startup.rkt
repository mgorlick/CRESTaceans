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
    (apply precall (cons f xs))
    (apply f xs)))

(define-syntax-rule (define-wrapper-for* f g postcall)
  (define (g . xs)
    (apply f xs)
    (apply postcall (cons f xs))))

(define sending-diagram-nodes (make-hash)) ; nickname -> node
(define sending-diagram-edges (make-hash)) ; (nickname . (nickname or island-address)) -> node
(define sending-diagram #f)

(define (add-new-edge ui origin target)
  (unless (hash-has-key? sending-diagram-edges (cons origin target))
    (when (and (hash-ref sending-diagram-nodes origin #f)
               (hash-ref sending-diagram-nodes target #f))
      (define edge (new-edge sending-diagram
                             (hash-ref sending-diagram-nodes origin)
                             (hash-ref sending-diagram-nodes target)
                             #t))
      (hash-set! sending-diagram-edges (cons origin target) edge)
      (ui-send! ui edge))))

(define (my-root-loop ui ui-notification-channel)
  (define tre (thread-receive-evt))
  (define e (sync tre ui-notification-channel))
  (define amsg (cond [(equal? tre e) (thread-receive)]
                     [else e]))
  ;(define amsg (thread-receive))
  (match amsg
    ;; spawn format, to be solely directed at the public curl.
    [(vector (? (curry equal? PUBLIC-CURL) pcurl) (match:spawn body metadata reply))
     (define the-nickname (gensym (or (metadata-ref metadata "nick") 'nonamegiven)))
     (define-values (actor actor/loc) (actor/new ROOT the-nickname))
     ; put a new node to represent the new computation.
     (define this-node (new-node sending-diagram (symbol->string the-nickname)))
     ; place in the map above
     (hash-set! sending-diagram-nodes the-nickname this-node)
     ; send to ui itself.
     (ui-send! ui this-node)
     ; a producer of functions that count and send a data point every N invocations.
     (define (make-counter thing-counted)
       (define N 10)
       (define count 0)
       (define this-chart #f)
       (lambda _
         (set! count (add1 count))
         (when (zero? (modulo count N))
           (unless this-chart 
             (set! this-chart 
                   (new-chart 'line (format "~a count of ~a" thing-counted the-nickname) "total"))
             (ui-send! ui this-chart))
           (ui-send! ui (plot-a-point this-chart (current-inexact-milliseconds) count)))))
     ; a producer of functions that create edges between `this-node' and other nodes
     ; when this computation sends messages to other computations.
     (define (make-edge-for target-curl)
       (cond [(curl/intra? target-curl)
              ; the receiver is also here.
              ; we should have another node for it on the diagram already (since it was already created)
              ; find it.
              (define target-actor (actor/nickname (locative/actor (curl/id target-curl))))
              ; now that we have both nodes draw the edge
              ; (if the edge doesn't exist.)
              (add-new-edge ui the-nickname target-actor)]
             [else ; off island.
              (define target-island-address (curl/island target-curl))
              ;(displayln target-island-address)
              ; make sure we have a node for the other island on the diagram
              (unless (hash-has-key? sending-diagram-nodes target-island-address)
                (define other-island-node (new-node sending-diagram
                                                    (string-append
                                                     (bytes->string/utf-8 (island/address/dns target-island-address))
                                                     ":"
                                                     (number->string (island/address/port target-island-address)))))
                (hash-set! sending-diagram-nodes target-island-address other-island-node)
                (ui-send! ui other-island-node))
              (add-new-edge ui the-nickname target-island-address)]))
     (define (draw-edge p c m . whatever)
       (cond
         [(eq? p base:curl/send/multiple)
          ; curl/send/multiple's arguments are backwards.
          ; `m' is actually a list of curls; `c' is actually the message.
          (map make-edge-for m)]
         [else 
          (make-edge-for c)]))
     ; make a counter for curl/send and friends.
     ; counter is shared between all the curl/send* functions
     (define counter-for-sends (make-counter "sending"))
     ; glue both of the curl/send instrumentations together.
     (define sending-instrumentation (λ xs 
                                       (apply counter-for-sends xs)
                                       (apply draw-edge xs)))
     (define-wrapper-for base:curl/send curl/send sending-instrumentation)
     (define-wrapper-for base:curl/send/multiple curl/send/multiple sending-instrumentation)
     (define-wrapper-for base:curl/send/promise curl/send/promise sending-instrumentation)
     (define benv (++ (metadata->benv metadata)
                      (global-value-defines the-nickname PUBLIC-CURL)
                      (global-defines this/locative curl/get-public 
                                      ; overwrite old versions with wrapped versions.
                                      curl/send 
                                      curl/send/promise
                                      curl/send/multiple)))
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


(serve-browser-ui (argsassoc "--ui" #:default 8000 #:no-val 8000 #:call string->number))

;;; start the root chieftain up.
(actor/jumpstart ROOT (λ ()                        
                        (define (open-a-tab/synch)
                          (define ac (make-async-channel))
                          (define s (open-a-tab (λ (json) (async-channel-put ac json))))
                          (values s ac))
                        (define-values (ui ui-notification-channel) (open-a-tab/synch)) 
                        (ui-wait-for-readiness ui)
                        (PROMISSARY ROOT)
                        (set! sending-diagram (new-diagram 1200 900))
                        (ui-send! ui sending-diagram)
                        ; put a new node to represent the new computation.
                        (define this-node (new-node sending-diagram "root"))
                        ; place in the map above
                        (hash-set! sending-diagram-nodes 'root this-node)
                        ; send to ui itself.
                        (ui-send! ui this-node)
                        (my-root-loop ui ui-notification-channel)))

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