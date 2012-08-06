#! /usr/bin/env racket
#lang racket/base

(require "motiles.rkt"
         "envs.rkt"
         "message-types.rkt"
         "../../ui/ws.rkt"
         "../../peer/src/api/framework.rkt" 
         "../../Motile/generate/baseline.rkt"
         "../../Motile/persistent/environ.rkt"
         "../../Motile/persistent/hash.rkt"
         "../../Motile/actor/actor.rkt"
         "../../Motile/actor/curl.rkt"
         (prefix-in base: "../../Motile/actor/send.rkt")
         "../../Motile/actor/delivery.rkt"
         "../../Motile/actor/promise.rkt"
         "../../Motile/actor/jumpstart.rkt"
         "../../Motile/actor/island.rkt"
         "../../Motile/actor/locative.rkt"
         racket/function
         racket/match
         racket/contract
         racket/async-channel)

(provide (all-defined-out))

(define/contract (metadata->benv metadata)
  (hash/persist? . -> . environ?)
  (cond [(meta-has-any? metadata accepts/webm) VIDEO-DECODE]
        [(meta-has-any? metadata produces/webm) VIDEO-ENCODE]
        [(meta-has-any? metadata is/gui) GUI]
        [(meta-has-any? metadata is/endpoint) GUI-ENDPOINT]
        [else MULTIMEDIA-BASE]))

;; host and port to listen on. use to start the comm layer below, designating root to receive incoming.
(define DNS (argsassoc "--host" #:no-val "127.0.0.1"))
(define PORT (argsassoc "--port" #:no-val 5000 #:call string->number))

(define ADDRESS-HERE 
  (island/address/new #"abcdefghijklmnopqrstuvwxyz" (string->bytes/utf-8 DNS) PORT))

(define-values (ROOT ROOT-LOCATIVE PUBLIC-LOCATIVE PUBLIC-CURL COMM-thd)
  (listen-on/make-root ADDRESS-HERE))
(printf "Island starting: ~s~n" (this/island))

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
                                       ;(apply counter-for-sends xs)
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
     (printf "Actor starting: ~s~n" the-nickname)
     (eval-actor actor body benv)
     (printf "Actor ending: ~s~n" the-nickname)]
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
  (define encoder-where@ 
    (curl/get-public (argsassoc "--vhost" #:no-val DNS #:default DNS)
                     (argsassoc "--vport" #:no-val PORT #:default PORT #:call string->number)))
  (define pubsub-where@ (if (equal? "dsite" (argsassoc "--psat")) PUBLIC-CURL encoder-where@))
  (define the-bang (motile/call make-big-bang
                                MULTIMEDIA-BASE
                                encoder-where@ device w h
                                pubsub-where@
                                PUBLIC-CURL))
  (base:curl/send PUBLIC-CURL (spawn/new the-bang (make-metadata (nick 'big-bang)) #f))
  (displayln "Spawn sent"))

(wait-forever)