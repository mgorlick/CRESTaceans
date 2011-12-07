#lang racket/base

(require "../../Motile/compile/compile.rkt")
(provide (all-defined-out))

;; notes on syntactic conventions to follow:
;; flub$ - a locative named flub
;; quux@ - a curl named quux
;; frobnicatorλ - the compiled source of a lambda `frobnicator' used as a motile actor
;; foo& - a box named foo
;; clax^ - a collection of metadata (i.e. produced with `make-metadata') named clax
;; blar# - a bytestring named `blar'
;; these aren't uniform yet as i have not refactored the entire module - please be patient :-)

; a video-specific big-bang takes a device name/width/height info along with two locations,
; spawns a proxy to connect a new decoder with a new encoder,
; then spawns the encoder and decoder.
(define (big-bang encoder-site-public-curl@ video-device video-w video-h decoder-site-public-curl@)
  (motile/compile
   `(letrec 
        ([f (lambda ()
              (define me@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
              ; spawn the proxy
              (curl/send ,encoder-site-public-curl@ (spawn/new (pubsubproxy me@)
                                                               (make-metadata is/proxy '(nick . pubsub))
                                                               #f))
              (let ([proxy@ (:remote/body (delivered/contents-sent (mailbox-get-message)))])
                ; spawn the encoder with the proxy as initialization address
                (curl/send ,encoder-site-public-curl@ (spawn/new (video-reader/encoder
                                                                  ,video-device ,video-w ,video-h proxy@)
                                                                 (make-metadata produces/webm '(nick . encoder))
                                                                 #f))
                ; spawn the decoder with the proxy as initialization address
                (curl/send ,decoder-site-public-curl@ (spawn/new (video-decoder/single proxy@)
                                                                 (make-metadata accepts/webm '(nick . decoder)) 
                                                                 #f))))])
      (f))))

;; pubsubproxy is a 1-to-N router with control signals forwarded backwards to the owner.
;; this implementation doesn't encode ownership (the "1" in "1-to-N") except by convention.
;; whoever sent some forward message is the one that will receive a control signal directed to it
;; (see implementation for exact forward message types and backwards control signals) 
(define (pubsubproxy on-birth-notify@)
  (motile/compile
   `(letrec 
        ([f (lambda ()
              ;(define me$ (this/locative))
              ;; create a serializable curl for both the publisher and the subscriber
              ;; to use.
              (define me@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
              ;; notify whoever I'm supposed to that I'm online now.
              (curl/send ,on-birth-notify@ (remote/new me@ '() #f))
              (let loop ([curls set/equal/null]
                         [last-sender-seen@ #f])
                (let ([m (delivered/contents-sent (mailbox-get-message))])
                  (cond [(remote? m)
                         (let ([body (:remote/body m)]
                               [rpyto@ (:remote/reply m)])
                           (cond
                             ;; the types that the router knows to send forward.
                             [(Frame? body)
                              (set/map curls (lambda (subber@)
                                               (curl/send subber@ (!:remote/reply m #f))))
                              (loop curls rpyto@)]
                             ;; the control messages coming from the forward direction,
                             ;; directed at the router.
                             [(AddCURL? body)
                              (displayln "New subscription")
                              (loop (set/cons curls (:AddCURL/curl body)) last-sender-seen@)]
                             [(RemoveCURL? body)
                              (loop (set/remove curls (:RemoveCURL/curl body)) last-sender-seen@)]
                             ;; the messages that the router is hardwired to send backward to the sender using it.
                             [(AddBehaviors? body)
                              (when last-sender-seen@ (curl/send last-sender-seen@ m))
                              (loop curls last-sender-seen@)]
                             [(Quit/MV? body)
                              (when last-sender-seen@ (curl/send last-sender-seen@ m))
                              (loop curls last-sender-seen@)]
                             [else 
                              (printf "proxy else: ~a~n" body)
                              (loop curls last-sender-seen@)]))]
                        [else (printf "A non-remote: ~a~n" m)
                              (loop curls last-sender-seen@)]))))])
      (f))))

;; -----
;; VIDEO
;; -----

;; a linker-bang takes a new sink to spawn, makes a pub sub proxy and starts up that
;; sink, concurrently sending the proxy's curl back to the existing source
(define (linker-bang)
  (motile/compile
   '(lambda (sink-site-public-curl@ sinkλ sink^ source@)
      (define me@ (curl/new/any (this/locative) null #f))
      ;; spawn a proxy to sit between the ESTABLISHED source and the NEW sink.
      #|(curl/send sink-site-public-curl@ (spawn/new (pubsubproxy me@)
                                                   (make-metadata is/proxy '(nick . pubsub))
                                                   #f))
      ;; get proxy curl back.
      (let ([proxy@ (:remote/body (delivered/contents-sent (mailbox-get-message)))])
        ;; link ESTABLISHED source and NEW sink to proxy.
        (curl/send source@ (remote/new proxy@ '() me@))
        (curl/send sink-site-public-curl@ (spawn/new (lambda () (sinkλ proxy@))
                                                     (make-metadata is/endpoint '(nick . canvas))
                                                     me@))))))|#
      (curl/send source@ (remote/new me@ '() me@))
      (sinkλ source@))))

;; a canvas endpoint is an actor that is responsible for video display and ONLY video display.
;; each decoding pipeline is allocated one or more gui endpoints.
(define (canvas-endpoint)
  (motile/compile
   '(lambda (on-birth-notify@ buffer-maker!)
      ;; curl used for receiving feed data should be pretty much unrestricted.
      ;; no need for serializability: publisher should be on-island anyway.
      ;; send the curl backwards along the pipeline to subscribe self to decoded feed.
      (let ([me/subscription@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t)
                                            null #f)])
        (curl/send on-birth-notify@ (remote/new (AddCURL/new me/subscription@) (make-metadata) #f))  
        (let* ([message1 (mailbox-get-message)]
               [pms (hash/ref (:remote/metadata (delivered/contents-sent message1)) 'params #f)])
          (let ([buffer# (buffer-maker! (:VideoParams/width pms) (:VideoParams/height pms))])
            (let loop ([m message1])
              (define body (:remote/body (delivered/contents-sent m)))
              (cond [(and buffer# (Frame? body))
                     (video-gui-update-buffer! buffer# (:Frame/data body))
                     (loop (mailbox-get-message))]
                    [else
                     (printf "Endpoint: unknown message ~a~n" body)
                     (loop (mailbox-get-message))]))))))))

(define (gui-controller)
  (motile/compile
   '(letrec 
        ([f (lambda ()
              ; curl used to allow decoders to look up this address upon their arrival on island.
              (define me/lookup@ (curl/new/any (this/locative) null #f))
              
              (define g (new-video-gui (curl/new/any (this/locative) null #f)))
              (define (spawn-gui-endpoint requester@)
                ;; !!! NO SERIALIZATION WARNING !!!
                ;; (we lifted `g' and `add-video!' out of the lambda)
                ;; a grown-up implementation would check that this
                ;; curl `requester@' is a local curl.
                (let* ([av! video-gui-add-video!]
                       [buffer-maker! (lambda (w h)
                                        (av! g w h requester@))]
                       [cnvs (canvas-endpoint)]
                       [cve (lambda (rpy@) ((cnvs) rpy@ buffer-maker!))]
                       [linker (linker-bang)])
                  ;; launch the linker to start up our wrapped endpoint
                  (curl/send PUBLIC/CURL (spawn/new (lambda () 
                                                      ((linker) PUBLIC/CURL cve (make-metadata) requester@))
                                                    (make-metadata  is/endpoint '(nick . canvas)) #f))))
              
              (set-current-gui-curl! me/lookup@)
              
              (let loop ([m (mailbox-get-message)]
                         [decoders set/equal/null])
                (define body (:remote/body (delivered/contents-sent m)))
                (cond 
                  ;; sent from new decoders.
                  [(AddCURL? body)
                   (spawn-gui-endpoint (:AddCURL/curl body))
                   (loop (mailbox-get-message) (set/cons decoders (:AddCURL/curl body)))]
                  ;; forwarded from the actual GUI.
                  [(RemoveCURL? body)
                   (curl/send (:RemoveCURL/curl body) (remote/new (Quit/new) (make-metadata) #f))
                   (loop (mailbox-get-message) (set/remove decoders (:RemoveCURL/curl body)))]
                  ;[(PIPOn? v)
                  ; (let ([the-pipλ ((unwrap (make-pip-decoder)) 
                  ;                  (list (PIPOn.major v) (PIPOn.minor v)))])
                  ;   (ask/send* "SPAWN" PUBLIC/CURL the-pipλ (make-metadata accepts/webm) PUBLIC/CURL))
                  ; (loop (mailbox-get-message) decoders)]
                  ;; copy all children then copy self.
                  [(CopyActor? body)
                   (curl/send (curl/get-public (:CopyActor/host body) (:CopyActor/port body))
                              (spawn/new f (make-metadata is/gui '(nick . gui-controller)) #f))
                   (set/map decoders (lambda (decoder@)
                                       (curl/send decoder@ (delivered/contents-sent m))))
                   (loop (mailbox-get-message) decoders)]
                  ;; only copy one child.
                  [(CopyChild? body)
                   (curl/send (:CopyChild/curl body) 
                              (remote/new (CopyActor/new (:CopyChild/host body) (:CopyChild/port body)) 
                                          (make-metadata) #f))
                   (loop (mailbox-get-message) decoders)]
                  ;; just pass backwards.
                  [(InitiateBehavior? body)
                   (curl/send (:InitiateBehavior/ref body) (delivered/contents-sent m))
                   (loop (mailbox-get-message) decoders)]
                  ;; probably something sent back beyond the decoder.
                  [(FwdBackward? body)
                   (curl/send (:FwdBackward/ref body) (delivered/contents-sent m))
                   (loop (mailbox-get-message) decoders)] 
                  ;; move decoders, then move self.
                  [(Quit/MV? body)
                   (curl/send (curl/get-public (:Quit/MV/host body) (:Quit/MV/port body))
                              (spawn/new f (make-metadata is/gui '(nick . gui-controller)) #f))
                   (set/map decoders (lambda (decoder@) (curl/send decoder@ (delivered/contents-sent m))))]
                  ;; pass on to decoders.
                  [(Quit? body)
                   (set/map decoders (lambda (decoder@) (curl/send decoder@ (delivered/contents-sent m))))]
                  [else
                   (printf "Not a valid request to GUI: ~a~n" body)
                   (loop (mailbox-get-message) decoders)])))])
      (f))))

(define (video-reader/encoder devname w h where-to-publish@)
  (motile/compile
   `(letrec 
        ([f (lambda ()
              (define me@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
              (define default-fudge 0.5) ; used as a FACTOR of the overall framerate.
              ;; i.e., a default-fudge of 0.5 at a camera-specified framerate of 20 fps results in a
              ;; waiting period of (1000/20) * 0.5 = 25 ms between the first and second capture attempt.
              (define fudge-step 0.01)
              (define vreader (video-reader-setup ,devname ,w ,h))
              (let* ([params (video-reader-get-params vreader)]
                     [framerate (bin/ (:VideoParams/fpsNum params) (:VideoParams/fpsDen params))])
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
                  (define webm+params^ (make-metadata type/webm (cons 'params params)))
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
                  (let ([m (mailbox-get-message)])
                    (define body (:remote/body (delivered/contents-sent m)))
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
                                      (spawn/new f (make-metadata produces/webm '(nick . encoder)) #f))]
                          [else 
                           ;; unknown message
                           (k fudge on-frame-callbacks)])))
                ;; main loop.
                (let loop ([fudge default-fudge]
                           [on-frame-callbacks (set/cons set/equal/null 
                                                         (make-encode-full-frame-cb ,where-to-publish@))])
                  (if (mailbox-has-message? (bin* fudge framerate)) ;; <--- !! AIMD-based waiting here !!
                      (do-control-message! loop fudge on-frame-callbacks)
                      (do-one-frame!       loop fudge on-frame-callbacks)))))])
      (f))))

(define (video-decoder/single where-to-subscribe@)
  (motile/compile
   `(letrec 
        ([f (lambda ()
              (define me/sub@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
              (define me/ctrl@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
              (define d (vp8dec-new))
              (displayln "Decoder on")
              ;; 1. look up the current GUI, ask for a new display function
              (curl/send (get-current-gui-curl) (remote/new (AddCURL/new me/ctrl@) (make-metadata) #f))
              (let ([gui-endpoint@ (:remote/body (delivered/contents-sent (mailbox-get-message)))])
                (displayln "Decoder subscribing")
                ;; 2. now that the decoder has a playback function it may decode frames, so ask for them.
                (curl/send ,where-to-subscribe@ (remote/new (AddCURL/new me/sub@) (make-metadata) #f))
                ;; 3. start decoding loop
                (let loop ()
                  (let ([m (mailbox-get-message)])
                    (define body (:remote/body (delivered/contents-sent m)))
                    (define desc^ (:remote/metadata (delivered/contents-sent m)))
                    (cond [(Frame? body)
                           (let* ([params (metadata-ref desc^ 'params)]
                                  [decoded-frame (vp8dec-decode-copy d 
                                                                     (:Frame/data body) 
                                                                     (:VideoParams/width params) 
                                                                     (:VideoParams/height params))])
                             (when decoded-frame
                               (curl/send gui-endpoint@ (!:remote/body (delivered/contents-sent m)
                                                                       (!:Frame/data body decoded-frame)))))
                           (loop)]
                          ;; following are messages send backwards across control flow path from controller.
                          [(GetParent? body)
                           (curl/send (:remote/reply m) (remote/new ,where-to-subscribe@ (make-metadata) #f))
                           (loop)]
                          ;;!!! FIXME !!!: this is doing connector work when it is a component.
                          [(FwdBackward? body)
                           (curl/send ,where-to-subscribe@ 
                                      (!:remote/body (delivered/contents-sent m) (:FwdBackward/msg body)))
                           (loop)]
                          [(CopyActor? body)
                           (curl/send (curl/get-public (:CopyActor/host body) (:CopyActor/port body))
                                      (spawn/new f (make-metadata accepts/webm '(nick . decoder)) #f))
                           (loop)]
                          [(Quit/MV? body)
                           (printf "Decoder removing self~n")
                           (curl/send ,where-to-subscribe@
                                      (remote/new (RemoveCURL/new me/ctrl@) (make-metadata) #f))
                           
                           (vp8dec-delete d)
                           (printf "Decoder doing a move+quit~n")
                           (curl/send (curl/get-public (:Quit/MV/host body) (:Quit/MV/port body))
                                      (spawn/new f (make-metadata accepts/webm '(nick . decoder)) #f))
                           (printf "Decoder shutting down after move+quit~n")]
                          [(Quit? body)
                           (curl/send ,where-to-subscribe@ (remote/new (RemoveCURL/new me/ctrl@) (make-metadata) #f))
                           (vp8dec-delete d)]
                          [else
                           (displayln "Decoder throwing away a message")
                           #;(printf "not a valid request to decoder: ~a~n" body)
                           (loop)])))))])
      (f))))