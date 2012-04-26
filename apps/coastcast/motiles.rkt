#lang racket/base

(require "../../Motile/compile/compile.rkt"
         "../../Motile/generate/baseline.rkt"
         "../../Motile/baseline.rkt")
(provide (all-defined-out))

(define (eval-definition e)
  (motile/call e BASELINE))
(define-syntax-rule (define-motile-procedure id body)
  (define id (eval-definition (motile/compile body))))

;; notes on syntactic conventions to follow:
;; flub$ - a locative named flub
;; quux@ - a curl named quux
;; frobnicatorλ - the compiled source of a lambda `frobnicator' used as a motile actor
;; foo& - a box named foo
;; clax^ - a collection of metadata (i.e. produced with `make-metadata') named clax
;; blar# - a bytestring named `blar'

; a video-specific big-bang takes a device name/width/height info along with two locations,
; spawns a proxy to connect a new decoder with a new encoder,
; then spawns the encoder and decoder.
(define-motile-procedure make-big-bang
  '(lambda (encoder-site-public-curl@ video-device video-w video-h ; encoder details.
                                      pubsub-site-public-curl@ ; where to put pubsub
                                      decoder-site-public-curl@) ; where to put decoder
     (lambda ()
       (define me@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
       ; spawn the proxy
       (curl/send pubsub-site-public-curl@ (spawn/new (make-encoder-side-relay me@)
                                                      (make-metadata is/proxy (nick 'encoder-side-relay))
                                                      #f))
       (let* ([proxys-list@ (delivery/contents-sent (mailbox-get-message))]
              [proxy-subscribable@ (car proxys-list@)]
              [proxy-publishable@ (cdr proxys-list@)])
         ; spawn the encoder with the proxy as initialization address
         (curl/send encoder-site-public-curl@ (spawn/new (make-video-reader/encoder video-device video-w video-h proxy-publishable@)
                                                         (make-metadata produces/webm (nick 'encoder))
                                                         #f))
         ; spawn the decoder with the proxy as initialization address
         (curl/send decoder-site-public-curl@ (spawn/new (make-video-decoder/single proxy-subscribable@)
                                                         (make-metadata accepts/webm (nick 'decoder)) 
                                                         #f))))))

(define-motile-procedure make-encoder-side-relay
  '(lambda (on-birth-notify@)
     (define (make-subscribable-curl l)
       (curl/new/any l '(subscribe) #f))
     (define (can-subscribe-with-curl? c)
       (member 'subscribe (curl/get-path c)))
     (define (make-publishable-curl l)
       (curl/new/any l '(publish) #f))
     (define (can-publish-with-curl? c)
       (member 'publish (curl/get-path c)))
     (define (make-subscription-control-curl l)
       (curl/new/any l '(subscription-control) (hash/new hash/eq/null 
                                                         'allowed 'remove 
                                                         'for-sub id)))
     (define (can-unsubscribe? c curls)
       (let ([meta (curl/get-meta c)]
             [path (curl/get-path c)])
         (and path meta 
              (member 'subscription-control path)
              (eq? 'remove (metadata-ref meta 'allowed))
              (hash/contains? curls (metadata-ref meta 'for-sub)))))
     (define (this)
       (define *me/ctrl (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t))
       (define me/subscribable@ (make-subscribable-curl *me/ctrl))
       (define me/publishable@ (make-publishable-curl *me/ctrl))
       ;; notify whoever I'm supposed to that I'm online now.
       (curl/send on-birth-notify@ (cons me/subscribable@ me/publishable@))
       (let loop ([forward-relays hash/equal/null] ; island-address => curl
                  [last-sender-seen@ #f])
         ;; unpack message
         (define m (mailbox-get-message))
         (define contents (delivery/contents-sent m))
         (define curl-used (delivery/curl-used m))
         (define promise-fulfillment (delivery/promise-fulfillment m))
         (define body (:remote/body contents))
         ;; process message
         (cond 
           ;; new publication: does it have the authority to publish?
           [(and (Frame? body)
                 (can-publish-with-curl? curl-used))
            (let ([content/hidden-location (!:remote/reply contents me/subscribable@)])
              ;; send frame to all forward relays.
              (hash/for-each forward-relays (lambda (id.subber@) (curl/send (cdr id.subber@) content/hidden-location))))
            (loop forward-relays (:remote/reply contents))]
           ;; new subscription request: does it have the authority to subscribe?
           [(and (AddCURL? body) 
                 (can-subscribe-with-curl? curl-used))
            (let* ([subscriber-island-address (curl/get-island-address (:AddCURL/curl body))])
              ;; is there a forward relay for this flow already deployed 
              ;; on the island on which the new subscriber is resident?
              (cond [(hash/contains? forward-relays subscriber-island-address)
                     ;; if so, forward this onto the forward relay
                     ;; allowing it to fulfill the subscriber's promise held
                     (curl/send (hash/ref forward-relays subscriber-island-address #f)
                                (remote/new body #f promise-fulfillment))
                     (loop forward-relays last-sender-seen@)]
                    [else
                     ;; if not, deploy a new forward relay
                     (let ([new-forward-relay-site@ 
                            (curl/get-public (island/address/get-dns subscriber-island-address)
                                             (island/address/get-port subscriber-island-address))]
                           [new-forward-relay-@-promise (promise/new 10000)])
                       (curl/send new-forward-relay-site@
                                  (spawn/new 
                                   ;; tell the new-forward relay to report its CURL back here
                                   (make-forward-relay (promise/to-fulfill new-forward-relay-@-promise))
                                   (make-metadata is/proxy (nick 'forward-relay))
                                   #f))
                       ;; get the CURL of the new forward relay
                       (let ([new-forward-relay@ (promise/wait (promise/result new-forward-relay-@-promise) 1 #f)])
                         ;; let the new forward relay fulfill the subscriber's promise
                         (cond ([(curl? new-forward-relay@)
                                 ;; now forward the subscription request onto the new forward relay.
                                 ;; allow the new forward relay to fulfill the original subscriber promise.
                                 (curl/send new-forward-relay@ (remote/new body #f promise-fulfillment))
                                 ;; add the new forward relay CURL to the list of forward relays
                                 (loop (hash/cons forward-relays subscriber-island-address new-forward-relay@)
                                       last-sender-seen@)]
                                ;; fulfill the subscriber's promise ourself notifying the subscriber
                                ;; of a problem on their island or the public CURL we have for their island
                                [else
                                 (when promise-fulfillment
                                   (curl/send promise-fulfillment 
                                              (error/new "couldn't spawn a forward relay on your island" m)))
                                 (loop forward-relays last-sender-seen@)]))))]))]
           [(and (RemoveCURL? body) (can-unsubscribe? curl-used))
            (displayln "RemoveCURL ok")
            (loop (hash/remove forward-relays (curl/get-island-address (:RemoveCURL/curl body)))
                  last-sender-seen@)]
           ;; the messages that the router is hardwired to send backward to the sender using it.
           [(AddBehaviors? body)
            (when last-sender-seen@ (curl/send last-sender-seen@ contents))
            (loop forward-relays last-sender-seen@)]
           [(Quit/MV? body)
            (when last-sender-seen@ (curl/send last-sender-seen@ contents))
            (loop forward-relays last-sender-seen@)]
           [else 
            (printf "encoder relay else: ~a~n" body)
            (loop forward-relays last-sender-seen@)])))
     this))

;; forward-relay is a 1-to-N router with control signals forwarded backwards to the owner.
;; this implementation doesn't encode ownership (the "1" in "1-to-N") except by convention.
;; whoever sent some forward message is the one that will receive a control signal directed to it
;; (see implementation for exact forward message types and backwards control signals) 
(define-motile-procedure make-forward-relay
  '(lambda (on-birth-notify@)
     (define (this)
       ;; create a serializable curl for both the publisher and the subscriber
       ;; to use.
       (define *me/ctrl (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t))
       (define me@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
       ;; notify whoever I'm supposed to that I'm online now.
       (curl/send on-birth-notify@ me@)
       (let loop ([curls hash/equal/null]
                  [last-sender-seen@ #f])
         (define m (mailbox-get-message))
         (define contents (delivery/contents-sent m))
         (define body (:remote/body contents))
         (cond
           ;; the types that the router knows to send forward.
           [(Frame? body)
            (hash/for-each curls 
                           (lambda (id.subber@) (curl/send (cdr id.subber@) contents)))
            (loop curls (:remote/reply contents))]
           ;; the control messages coming from the forward direction,
           ;; directed at the router.
           [(AddCURL? body)
            (let ([id (make-uuid)]
                  [reply-to (:remote/reply contents)])
              ;; upon subscription, issue a CURL that lets the holder
              ;; interact with that subscription.
              (when reply-to
                (curl/send reply-to
                           (remote/new (curl/new/any *me/ctrl
                                                     null
                                                     (hash/new hash/eq/null
                                                               'allowed 'remove
                                                               'for-sub id))
                                       null #f)))
              ;(printf "Adding ~a ~n => ~a~n" id (:AddCURL/curl body))
              ;(displayln "to")
              ;(printf "~a~n" curls)
              (loop (hash/cons curls id (:AddCURL/curl body)) last-sender-seen@))]
           [(RemoveCURL? body)
            (let ([meta (curl/get-meta (delivery/curl-used m))])
              ;; look for a CURL that allows subscription interaction.
              (cond [(and meta 
                          (eq? 'remove (metadata-ref meta 'allowed))
                          (hash/contains? curls (metadata-ref meta 'for-sub)))
                     (loop (hash/remove curls (metadata-ref meta 'for-sub)) last-sender-seen@)]
                    ;; disallow subscription tinkering otherwise.
                    [else
                     (loop curls last-sender-seen@)]))]
           ;; the messages that the router is hardwired to send backward to the sender using it.
           [(AddBehaviors? body)
            (when last-sender-seen@ (curl/send last-sender-seen@ contents))
            (loop curls last-sender-seen@)]
           [(Quit/MV? body)
            (when last-sender-seen@ (curl/send last-sender-seen@ contents))
            (loop curls last-sender-seen@)]
           [else 
            (printf "forward relay else: ~a~n" body)
            (loop curls last-sender-seen@)])))
     this))

;; -----
;; VIDEO
;; -----

;; a linker-bang takes a new sink to spawn, makes a pub sub proxy and starts up that
;; sink, concurrently sending the proxy's curl back to the existing source
(define-motile-procedure linker-bang
  '(lambda (sink-site-public-curl@ sinkλ sink^ source@)
     (define me@ (curl/new/any (this/locative) null #f))
     ;; spawn a proxy to sit between the ESTABLISHED source and the NEW sink.
     (curl/send sink-site-public-curl@ (spawn/new (make-forward-relay me@)
                                                  (make-metadata is/proxy (nick 'forward-relay))
                                                  #f))
     ;; get relay curl back.
     (let ([relay@ (delivery/contents-sent (mailbox-get-message))])
       ;; link ESTABLISHED source and NEW sink to proxy.
       (curl/send source@ (remote/new relay@ '() me@))
       (curl/send sink-site-public-curl@ (spawn/new (lambda () (sinkλ relay@))
                                                    sink^
                                                    me@)))))

;; a canvas endpoint is an actor that is responsible for video display and ONLY video display.
;; each decoding pipeline is allocated one or more gui endpoints.
(define-motile-procedure canvas-endpoint
  '(lambda (on-birth-notify@ buffer-maker!)
     ;; curl used for receiving feed data should be pretty much unrestricted.
     ;; no need for serializability: publisher should be on-island anyway.
     ;; send the curl backwards along the pipeline to subscribe self to decoded feed.
     (let ([me/subscription@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t)
                                           null #f)])
       (curl/send on-birth-notify@ (remote/new (AddCURL/new me/subscription@) (make-metadata) #f))  
       (let* ([message1 (mailbox-get-message)])
         (define pms (metadata-ref (:remote/metadata (delivery/contents-sent message1)) "params"))
         (define buffer# (buffer-maker! (:VideoParams/width pms) (:VideoParams/height pms)))
         (let loop ([m message1])
           (define body (:remote/body (delivery/contents-sent m)))
           (cond [(and buffer# (Frame? body))
                  (video-gui-update-buffer! buffer# (:Frame/data body))
                  (loop (mailbox-get-message))]
                 [else
                  (printf "Endpoint: unknown message ~a~n" body)
                  (loop (mailbox-get-message))]))))))

(define-motile-procedure gui-controller
  '(let ()
     (define (this)
       ; curl used to allow decoders to look up this address upon their arrival on island.
       (define me/lookup@ (curl/new/any (this/locative) null #f))
       
       (define g (new-video-gui (curl/new/any (this/locative) null #f)))
       (define (spawn-gui-endpoint to-notify@ to-ctrl@)
         ;; !!! NO SERIALIZATION WARNING !!!
         ;; (we lifted `g' and `add-video!' out of the lambda)
         (define av! video-gui-add-video!)
         (define (buffer-maker! w h) (av! g w h to-ctrl@))
         (define (the-canvas-fun on-birth-notify@) (canvas-endpoint on-birth-notify@ buffer-maker!))
         (and (curl/intra? to-notify@)
              ;; launch the linker to start up our wrapped endpoint
              (curl/send PUBLIC-CURL 
                         (spawn/new (lambda ()
                                      (linker-bang PUBLIC-CURL
                                                   the-canvas-fun
                                                   (make-metadata is/endpoint (nick 'canvas))
                                                   to-notify@))
                                    (make-metadata (nick 'linker)) #f))))
       
       (set-current-gui-curl! me/lookup@)
       
       (let loop ([decoders set/equal/null])
         (define m (mailbox-get-message))
         (define body (:remote/body (delivery/contents-sent m)))
         (cond 
           ;; sent from new decoders.
           [(AddCURL? body)
            (unless (spawn-gui-endpoint (delivery/promise-fulfillment m) (:AddCURL/curl body))
              (curl/send (:AddCURL/curl body)
                         (error/new 'not-on-same-island (delivery/contents-sent m))))
            (loop (set/cons decoders (:AddCURL/curl body)))]
           ;; forwarded from the actual GUI.
           [(RemoveCURL? body)
            (curl/send (:RemoveCURL/curl body) (remote/new (Quit/new) (make-metadata) #f))
            (loop (set/remove decoders (:RemoveCURL/curl body)))]
           [(PIPOn? body)
            (curl/send PUBLIC-CURL
                       (spawn/new (make-video-decoder/pip (:PIPOn/major body) (:PIPOn/minor body))
                                  (make-metadata accepts/webm (nick 'pipdec))
                                  #f))
            (loop decoders)]
           ; copy all children then copy self.
           [(CopyActor? body)
            (curl/send (curl/get-public (:CopyActor/host body) (:CopyActor/port body))
                       (spawn/new this (make-metadata is/gui (nick 'gui-controller)) #f))
            (set/map decoders (lambda (decoder@)
                                (curl/send decoder@ (delivery/contents-sent m))))
            (loop decoders)]
           ;; only copy one child.
           [(CopyChild? body)
            (curl/send (:CopyChild/curl body) 
                       (remote/new (CopyActor/new (:CopyChild/host body) (:CopyChild/port body)) 
                                   (make-metadata) #f))
            (loop decoders)]
           ;; just pass backwards.
           [(InitiateBehavior? body)
            (curl/send (:InitiateBehavior/ref body) (delivery/contents-sent m))
            (loop decoders)]
           ;; probably something sent back beyond the decoder.
           [(FwdBackward? body)
            (curl/send (:FwdBackward/ref body) (delivery/contents-sent m))
            (loop decoders)]
           ;; move decoders, then move self.
           [(Quit/MV? body)
            (reset-current-gui-curl!)
            (curl/send (curl/get-public (:Quit/MV/host body) (:Quit/MV/port body))
                       (spawn/new this (make-metadata is/gui (nick 'gui-controller)) #f))
            (set/map decoders (lambda (decoder@) (curl/send decoder@ (delivery/contents-sent m))))]
           ;; pass on to decoders.
           [(Quit? body)
            (reset-current-gui-curl!)
            (set/map decoders (lambda (decoder@) (curl/send decoder@ (delivery/contents-sent m))))]
           [else
            (printf "Not a valid request to GUI: ~a~n" body)
            (loop decoders)])))
     this))

(define-motile-procedure make-video-reader/encoder
  '(lambda (devname w h where-to-publish@)
     (define (this)
       (define me@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
       (define default-fudge 0.5) ; used as a FACTOR of the overall framerate.
       ;; i.e., a default-fudge of 0.5 at a camera-specified framerate of 20 fps results in a
       ;; waiting period of (1000/20) * 0.5 = 25 ms between the first and second capture attempt.
       (define fudge-step 0.01)
       (define vreader (video-reader-setup devname w h))
       (define params (video-reader-get-params vreader))
       (define framerate (bin/ (:VideoParams/fpsNum params) (:VideoParams/fpsDen params)))
       ; each on-frame callback should obey the following:
       ; if `fb' is #f then clean up any resources. the host video reader
       ; promises that the callback will not be called again without reinitializing.
       (define (make-callback type f)
         (define e ((if (equal? type 'full) vp8enc-new vp8enc-quartersize-new) params))
         (define outbuff# (make-bytes (bin* 1024 256)))
         (lambda (fb)
           (if fb
               (f e outbuff# fb)
               (vp8enc-delete e))))
       ; the default callback: a full-frame encoding.
       (define (make-encode-full-frame-cb target@)
         (define webm+params^ (make-metadata type/webm `("params" . ,params)))
         (define (compress-full-frame e outbuff# fb)
           (define encoded# (vp8enc-encode e fb outbuff#))
           (when encoded#
             (curl/send target@ (remote/new (FrameBuffer->Frame encoded#) webm+params^ me@))))
         (make-callback 'full compress-full-frame))
       ; do-one-frame!: featuring AIMD waiting for camera frames.
       ; 1. try to read the next frame.
       ; 2a. if the frame was successfully read and encoded, send it off to the proxy curl.
       ;     then, loop with an additive decrease in waiting time till the next frame.
       ; 2b. if the frame was NOT successfully read and encoded, multiplicatively increase
       ;     the waiting time til the next frame. 
       ;     this seems to give relatively constant framerate overall, with few instances of
       ;     many "misses" in a row, where a "miss" is checking the camera before it is ready.
       (define (do-one-frame! k fudge on-frame-callbacks)
         (define fb (and (video-reader-is-ready? vreader)
                         (video-reader-get-frame vreader (current-inexact-milliseconds))))
         (cond [fb
                ; FrameBuffer received successfully. Pass it on and then wait until
                ; the next frame should be active (modified by a factor to account for processing time)                
                (set/map on-frame-callbacks (lambda (f) (f fb)))
                
                (dispose-FrameBuffer fb)
                ; decrease fudge factor for next frame a la AIMD.
                (k (if (bin>= fudge fudge-step) (bin- fudge fudge-step) 0) on-frame-callbacks)]
               [else
                ; increase fudge factor for next frame a la AIMD.
                (k (min* default-fudge (bin* 2 (max* fudge-step fudge))) on-frame-callbacks)]))
       ;; do a control message check when the encoder receives any message.
       (define (do-control-message! k fudge on-frame-callbacks)
         (define body (:remote/body (delivery/contents-sent (mailbox-get-message))))
         (cond [(AddBehaviors? body)
                ;; add new behaviors to the set of behaviors.
                (k fudge (foldl (lambda (behavior-ctor cbs)
                                  (set/cons cbs (behavior-ctor params make-callback)))
                                on-frame-callbacks
                                (AddBehaviors.new-behaviors body)))]
               [(Quit/MV? body)
                (video-reader-delete vreader)
                ;; clean up all the resources associated with a behavior (f #f)
                (set/map on-frame-callbacks (lambda (f) (f #f)))
                (curl/send (curl/get-public (:Quit/MV/host body) (:Quit/MV/port body))
                           (spawn/new this (make-metadata produces/webm (nick 'encoder)) #f))]
               [else 
                ;; unknown message
                (k fudge on-frame-callbacks)]))
       ;; main loop.
       (let loop ([fudge default-fudge]
                  [on-frame-callbacks (set/cons set/equal/null 
                                                (make-encode-full-frame-cb where-to-publish@))])
         (if (mailbox-has-message? (bin* fudge framerate)) ;; <--- !! AIMD-based waiting here !!
             (do-control-message! loop fudge on-frame-callbacks)
             (do-one-frame!       loop fudge on-frame-callbacks))))
     this))

(define-motile-procedure make-video-decoder/single
  '(lambda (where-to-subscribe@)
     (define (this)
       (define me/sub@ (curl/new/any 
                        (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
       (define me/ctrl@ (curl/new/any 
                         (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
       (define d (vp8dec-new))
       (define (reserve-gui/get-next-pipeline@)
         (curl/send/promise (get-current-gui-curl) 
                            (remote/new (AddCURL/new me/ctrl@) (make-metadata) #f)
                            10000))
       (define (add-sub upstream@)
         (curl/send/promise upstream@ 
                            (remote/new (AddCURL/new me/sub@) (make-metadata) #f)
                            10000))
       (define (unpack-promise p); generic way to wait for a promise and look at its final-value body.
         (:remote/body (promise/wait p 10000 #f)))
       ;; 1. reserve display.
       ;; 2. add subscription.
       (define gui-endpoint@ (unpack-promise (reserve-gui/get-next-pipeline@)))
       (define my-sub/ctrl@ (unpack-promise (add-sub where-to-subscribe@)))
       ;; 3. start decoding loop
       (let loop ()
         (define m (mailbox-get-message))
         (define body (:remote/body (delivery/contents-sent m)))
         (define desc^ (:remote/metadata (delivery/contents-sent m)))
         (define reply@ (:remote/reply (delivery/contents-sent m)))
         (cond [(Frame? body)
                (let* ([params (metadata-ref desc^ "params")]
                       [decoded-frame (vp8dec-decode-copy d 
                                                          (:Frame/data body) 
                                                          (:VideoParams/width params) 
                                                          (:VideoParams/height params))])
                  (when decoded-frame
                    (curl/send gui-endpoint@ (!:remote/body (delivery/contents-sent m)
                                                            (!:Frame/data body decoded-frame)))))
                (loop)]
               ;; following are messages send backwards across control flow path from controller.
               [(GetParent? body)
                (curl/send (delivery/promise-fulfillment m)
                           (remote/new where-to-subscribe@ (make-metadata) #f))
                (loop)]
               ;;!!! FIXME !!!: this is doing connector work when it is a component.
               [(FwdBackward? body)
                (curl/send where-to-subscribe@ 
                           (!:remote/body (delivery/contents-sent m) (:FwdBackward/msg body)))
                (loop)]
               [(CopyActor? body)
                (curl/send (curl/get-public (:CopyActor/host body) (:CopyActor/port body))
                           (spawn/new this (make-metadata accepts/webm (nick 'decoder)) #f))
                (loop)]
               [(Quit/MV? body)
                (curl/send my-sub/ctrl@
                           (remote/new (RemoveCURL/new) (make-metadata) #f))
                
                (vp8dec-delete d)
                (curl/send (curl/get-public (:Quit/MV/host body) (:Quit/MV/port body))
                           (spawn/new this (make-metadata accepts/webm (nick 'decoder)) #f))]
               [(Quit? body)
                (curl/send my-sub/ctrl@
                           (remote/new (RemoveCURL/new) (make-metadata) #f))
                (vp8dec-delete d)]
               [else
                (displayln "Decoder throwing away a message")
                (loop)])))
     this))

(define-motile-procedure make-video-decoder/pip
  '(lambda (majordec@ minordec@)
     (define (unpack-promise p) ; generic way to wait for a promise and look at its final-value body.
       (:remote/body (promise/wait p 1000 #f)))
     (define (retrieve-sink me@) ; request a canvas to draw on from the GUI.
       (unpack-promise (curl/send/promise (get-current-gui-curl) (remote/new (AddCURL/new me@) '() #f) 1000)))
     (define (retrieve-proxy-from dcurl@) ; talk to an existing decoder to get its upstream contact point.
       (unpack-promise (curl/send/promise dcurl@ (remote/new (GetParent/new) null #f)
                                                        1000)))
     (define (add-self-subscription prox@ me@) ; subscribe to an upstream contact point.
       (unpack-promise (curl/send/promise prox@ (remote/new (AddCURL/new me@) '() #f) 1000)))
     (define (remove-self-subscription prox@) ; remove subscription from an upstream contact point.
       (curl/send prox@ (remote/new (RemoveCURL/new) '() #f)))
     (define (spawn-single-decoder prox@) ; spawn a decoder that consumes one of the two feeds.
       (curl/send PUBLIC-CURL (spawn/new (make-video-decoder/single prox@)
                                         (make-metadata accepts/webm (nick 'decoder))
                                         #f)))
     (define cleanup-decs (lambda decs (map vp8dec-delete decs)))
     (define cleanup-subs (lambda proxs@ (for-each (lambda (p@) (and p@ (remove-self-subscription p@))) proxs@)))
     
     (define majorprox@ (retrieve-proxy-from majordec@))
     (define minorprox@ (retrieve-proxy-from minordec@))
     
     (define (this)
       (define (respawn-self where/p where/h major@ minor@) ; spawn a copy of self assuming the liveness of the
         ; designated island and of the two upstream contact points.
         (define where@ (curl/get-public where/p where/h))
         (curl/send where@ (spawn/new 
                            this
                            (make-metadata accepts/webm (nick 'pipdec)) #f)))
       (define me@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t)
                                 null #f))
       (define next-pipeline-element@ (retrieve-sink me@))
       (define flow1/control@ (add-self-subscription majorprox@ me@))
       (define flow2/control@ (and (not (curl/target=? majorprox@ minorprox@))
                                   (add-self-subscription minorprox@ me@)))
       (let loop ([decoder/major (vp8dec-new)]
                  [decoder/minor (vp8dec-new)]
                  [last-decoded-frame #f]
                  [majorprox@ majorprox@] 
                  [minorprox@ minorprox@]
                  [m (mailbox-get-message)])
         (define body (:remote/body (delivery/contents-sent m)))
         (define desc^ (:remote/metadata (delivery/contents-sent m)))
         (define replyaddr@ (:remote/reply (delivery/contents-sent m)))
         (cond 
           [(Frame? body)
            ;; these two steps are really a fold but Motile doesn't have arbitrary-arity folds
            ;; so they're just expressed this way for now.
            (let* ([frame-after-major-check
                    (cond [(and (curl/target=? replyaddr@ majorprox@) (not last-decoded-frame))
                           ;; no prior frame. decode a new one and save it but only if 
                           ;; this frame is a header-carrying major frame.
                           ;; OK to try to decode (might not work this time if 
                           ;; this isn't a header-carrying frame)
                           (let* ([this-frame-w (:VideoParams/width (metadata-ref desc^ "params"))]
                                  [this-frame-d (:VideoParams/height (metadata-ref desc^ "params"))])
                             (vp8dec-decode-copy decoder/major 
                                                 (:Frame/data body) this-frame-w this-frame-d))]
                          ;; have prior frame and stream is major. update over prior frame
                          [(and (curl/target=? replyaddr@ majorprox@) last-decoded-frame)
                           (vp8dec-decode-update-major decoder/major decoder/minor
                                                       (:Frame/data body) last-decoded-frame)]
                          ;; frame is minor stream only, or some other stream. ignore
                          [else last-decoded-frame])]
                   
                   [frame-after-minor-check
                    (cond [(and frame-after-major-check (curl/target=? replyaddr@ minorprox@))
                           (vp8dec-decode-update-minor decoder/major decoder/minor 
                                                       (:Frame/data body) frame-after-major-check)]
                          ;; frame is major stream only, or some other stream. ignore
                          [else
                           frame-after-major-check])])
              ;; if there is a frame to send out, send it down the pipeline
              (when frame-after-minor-check
                (curl/send next-pipeline-element@ 
                           (!:remote/body (delivery/contents-sent m)
                                          (!:Frame/data body frame-after-minor-check))))
              ;; finished with this frame.
              (loop decoder/major decoder/minor frame-after-minor-check
                    majorprox@ minorprox@ (mailbox-get-message)))]
           ;; following are control messages passed from a controller backwards along the pipeline
           ;; to this decoder.
           [(and (InitiateBehavior? body) (eq? 'toggle-major/minor (:InitiateBehavior/type body)))
            ; fixme: right now this deletes old states, makes fresh ones.
            ; with some tinkering they could be reused 
            ; (there is a strange state issue to investigate)
            (cleanup-decs decoder/major decoder/minor)
            (loop (vp8dec-new) (vp8dec-new) #f minorprox@ majorprox@ (mailbox-get-message))]
           [(and (InitiateBehavior? body) (eq? 'split (:InitiateBehavior/type body)))
            ; to split just spawn two new fullscreen decoders.
            (map spawn-single-decoder (list majorprox@ minorprox@))
            (loop decoder/major decoder/minor last-decoded-frame
                  majorprox@ minorprox@ (mailbox-get-message))]
           [(GetParent? body)
            ; assumes parents are proxies. PIP can send this to itself/other PIP
            (curl/send (delivery/promise-fulfillment m)
                       (remote/new (cons majorprox@ minorprox@) null #f))
            (loop decoder/major decoder/minor last-decoded-frame 
                  majorprox@ minorprox@ (mailbox-get-message))]
           ;; spawn a new copy of this enclosing lambda expression...
           [(CopyActor? body)
            (respawn-self (:CopyActor/host body) (:CopyActor/port body) majorprox@ minorprox@)
            (loop decoder/major decoder/minor last-decoded-frame
                  majorprox@ minorprox@ (mailbox-get-message))]
           ;; same as above, but with a cleanup after
           [(Quit/MV? body)
            (respawn-self (:Quit/MV/host body) (:Quit/MV/port body) majorprox@ minorprox@)
            (cleanup-decs decoder/major decoder/minor)
            (cleanup-subs flow1/control@ flow2/control@)]
           [(Quit? body)
            (cleanup-decs decoder/major decoder/minor)
            (cleanup-subs flow1/control@ flow2/control@)]
           [else
            (printf "not a valid request to PIP decoder: ~a~n" body)
            (loop decoder/major decoder/minor last-decoded-frame 
                  majorprox@ minorprox@ (mailbox-get-message))])))
     this))