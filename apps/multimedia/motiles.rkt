#lang racket/base

(require "../../Motile/compile/compile.rkt")
(provide (all-defined-out))

;; notes on syntactic conventions to follow:
;; quux@ - a curl named quux
;; frobnicatorλ - the compiled source of a lambda `frobnicator' used as a motile actor
;; foo& - a box named foo
;; clax^ - a collection of metadata (i.e. produced with `make-metadata') named clax
;; blar# - a bytestring named `blar'
;; these aren't uniform yet as i have not refactored the entire module - please be patient :-)

; a video-specific big-bang takes a device name/width/height info along with two locations,
; spawns a proxy to connect a new decoder with a new encoder,
; then spawns the encoder and decoder.
(define (big-bang encoder-location@ video-device video-w video-h decoder-location@)
  (motile/compile
   `(lambda ()
      ; spawn the proxy
      (ask/send* "SPAWN" ,encoder-location@ (make-pubsubproxy) (make-metadata is/proxy))
      (let ([proxy@ (car (thread-receive))])
        ; spawn the encoder with the proxy as reply addr
        (ask/send* "SPAWN" ,encoder-location@
                   (make-video-reader/encoder ,video-device ,video-w ,video-h)
                   (make-metadata produces/webm) proxy@)
        ; spawn the decoder with the proxy as reply addr
        (ask/send* "SPAWN" ,decoder-location@ (make-single-decoder)
                   (make-metadata accepts/webm) proxy@)))))


;; a linker-bang takes a new sink to spawn, makes a pub sub proxy and starts up that
;; sink, concurrently sending the proxy's curl back to the existing source
(define linker-bang
  (motile/compile
   '(lambda (sink-island-root@ sinkλ sink^ source@)
      (displayln "Spawning pubsub")
      (ask/send* "SPAWN" sink-island-root@ (make-pubsubproxy) (make-metadata is/proxy))
      (let ([proxy-curl (car (thread-receive))])
        (displayln "Spawning sink and notifying source")
        (ask/send* "SPAWN" sink-island-root@ (lambda () (sinkλ proxy-curl)) sink^ proxy-curl)
        (ask/send* "POST" source@ proxy-curl)))))

;; pubsubproxy is a 1-to-N router with control signals forwarded backwards to the owner.
;; this implementation doesn't encode ownership (the "1" in "1-to-N") except by convention.
;; whoever sent some forward message is the one that will receive a control signal directed to it
;; (see implementation for exact forward message types and backwards control signals) 
(define pubsubproxy
  (motile/compile
   `(lambda ()
      (displayln "Proxy on")
      (let loop ([curls set/equal/null]
                 [last-sender-seen@ #f])
        (let ([m (thread-receive)])
          (define v (car m))
          (define r (cdr m))
          (cond
            ;; the types that the router knows to send forward.
            [(Frame? v)
             (set/map curls (lambda (crl@) (ask/send* "POST" crl@ v (:message/ask/metadata r))))
             (loop curls (:message/ask/reply r))]
            ;; the control messages coming from the forward direction,
            ;; directed at the router.
            [(AddCURL? v)
             (loop (set/cons curls (AddCURL.curl v)) last-sender-seen@)]
            [(RemoveCURL? v)
             (ask/send* "POST" (RemoveCURL.curl v) (Quit) (:message/ask/metadata r))
             (loop (set/remove curls (RemoveCURL.curl v)) last-sender-seen@)]
            ;; the messages that the router is hardwired to send backward to the sender using it.
            [(AddBehaviors? v)
             (when last-sender-seen@
               (ask/send* "POST" last-sender-seen@ v))
             (loop curls last-sender-seen@)]
            [(Quit/MV? v)
             (when last-sender-seen@
               (ask/send* "POST" last-sender-seen@ v))
             (loop curls last-sender-seen@)]
            [else 
             (printf "proxy else: ~a~n" v)
             (loop curls last-sender-seen@)]))))))

;; -----
;; VIDEO
;; -----

;; a canvas endpoint is an actor that is responsible for video display and ONLY video display.
;; each decoding pipeline is allocated one or more gui endpoints.
(define canvas-endpoint
  (motile/compile
   '(lambda (rpy@ buffer-maker!)
      (ask/send* "POST" rpy@ (AddCURL (current-curl)))
      (let ([buffer& (box #f)])
        (let loop ([m (thread-receive)])
          (define v (car m))
          (define r (cdr m))
          (cond [(and (not (unbox buffer&)) (Frame? v))
                 (let ([params (cdr (assoc "params" (:message/ask/metadata r)))])
                   (box! buffer& (buffer-maker! (VideoParams.width params) (VideoParams.height params)))
                   (loop m))]
                [(and (unbox buffer&) (Frame? v))
                 (video-gui-update-buffer! (unbox buffer&) (Frame.data v))
                 (loop (thread-receive))]
                [else
                 (loop (thread-receive))]))))))

(define command-center-gui
  (motile/compile
   '(lambda (reply-curl@)
      (set-current-gui-curl! (current-curl))
      (let ()
        (define g (new-video-gui))
        
        (define (spawn-gui-endpoint requester@)
          ;; !!! NO SERIALIZATION WARNING !!!
          ;; (we lifted `g' and `add-video!' out of the lambda)
          ;; a grown-up implementation would check that this
          ;; curl `requester@' is a local curl.
          (let* ([add-video! video-gui-add-video!]
                 [endpointλ (unwrap (make-canvas-endpoint))]
                 [endpoint*λ (lambda (rpy)
                               (endpointλ rpy 
                                          (lambda (w h)
                                            (add-video! g w h requester@))))]
                 [root-here@ (get-root-curl)]
                 [linker!λ (unwrap (make-linker-bang))])
            (ask/send* "SPAWN" (get-root-curl)
                       (lambda ()
                         ;; launch the linker to start up our wrapped endpoint
                         (linker!λ root-here@ endpoint*λ (make-metadata is/endpoint) requester@))
                       (make-metadata))))
        (let loop ([m (thread-receive)]
                   [decoders set/equal/null])
          (define v (car m))
          (define r (cdr m))
          (cond 
            ;; sent from new decoders.
            [(AddCURL? v)
             (spawn-gui-endpoint (AddCURL.curl v))
             (loop (thread-receive) (set/cons decoders (AddCURL.curl v)))]
            ;; forwarded from the actual GUI.
            [(RemoveCURL? v)
             (ask/send* "DELETE" (RemoveCURL.curl v) (Quit))
             (loop (thread-receive) 
                   (set/remove decoders (RemoveCURL.curl v)))]
            [(PIPOn? v)
             (let ([the-pipλ ((unwrap (make-pip-decoder)) 
                              (list (PIPOn.major v) (PIPOn.minor v)))])
               (ask/send* "SPAWN" (get-root-curl) the-pipλ (make-metadata accepts/webm) (get-root-curl)))
             (loop (thread-receive) decoders)]
            [(CP? v)
             (respawn-self (CP.host v) (CP.port v))
             (set/map decoders (lambda (decoder) 
                                 (ask/send* "POST" decoder v)))
             (loop (thread-receive) decoders)]
            [(FwdBackward? v)
             (ask/send* "POST" (FwdBackward.ref v) v)
             (loop (thread-receive) decoders)]
            [(CP-child? v)
             (ask/send* "POST" (CP-child.curl v) (CP (CP-child.host v) (CP-child.port v)))
             (loop (thread-receive) decoders)]
            [(InitiateBehavior? v)
             (ask/send* "POST" (InitiateBehavior.ref v) v)
             (loop (thread-receive) decoders)]
            [(Quit/MV? v)
             (respawn-self (Quit/MV.host v) (Quit/MV.port v))
             (set/map decoders (lambda (decoder) (ask/send* "DELETE" decoder v)))]
            [(Quit? v)
             (set/map decoders (lambda (decoder) (ask/send* "DELETE" decoder v)))]
            [else
             (printf "Not a valid request to GUI: ~a~n" v)
             (loop (thread-receive) decoders)]))))))

(define video-decoder/pip
  (motile/compile
   '(lambda (decoder-curls)
      ;; helper functions.
      ;; do the decoder half of the decoder/GUI discovery protocol.
      ;; -> curl
      (define (retrieve-sink)
        (ask/send* "POST" (get-current-gui-curl) (AddCURL (current-curl)))
        (car (thread-receive)))
      ;; given the curl of a decoder ask for its proxy's curl.
      ;; curl -> curl
      (define (retrieve-proxy-from dcurl@)
        (ask/send* "POST" dcurl@ (GetParent))
        (car (thread-receive)))
      ;; get the width/height of a message with a VideoParams as a metadata field (name `params')
      (define (get-w/h m)
        (define params (cdr (assoc "params" (:message/ask/metadata (cdr m)))))
        (cons (VideoParams.width params) (VideoParams.height params)))
      ;; add the current curl to a proxy prox@'s subscription list
      (define (add-self-subscription prox@)
        (ask/send* "POST" prox@ (AddCURL (current-curl))))
      ;; remove the current curl from a proxy prox@'s subscription list
      (define (remove-self-subscription prox@)
        (ask/send* "POST" prox@ (RemoveCURL (current-curl))))
      ;; spawn single decoder: given the proxy of a feed,
      ;; spawn a fullscreen decoder that will subscribe to it
      (define (spawn-single-decoder prox@)
        (ask/send* "SPAWN" (get-root-curl) (make-single-decoder) (make-metadata accepts/webm) prox@))
      ;; here's all the decoders we will use for the picture composition.
      (let ([proxy-curls (map retrieve-proxy-from decoder-curls)])
        (lambda ()
          (define next-pipeline-element@ (retrieve-sink))
          (define majorprox@ (car proxy-curls))
          (define minorprox@ (cadr proxy-curls))
          ;; let the proxies know we're online...
          (map add-self-subscription (list majorprox@ minorprox@))
          (let loop ([decoder/major (vp8dec-new)] 
                     [decoder/minor (vp8dec-new)]
                     [last-decoded-frame #f]
                     [majorprox@ majorprox@] 
                     [minorprox@ minorprox@]
                     [m (thread-receive)])
            (define v (car m))
            (define r (cdr m))
            (cond 
              [(Frame? v)
               (let* ([replyaddr@ (:message/ask/reply r)]
                      ;; these two steps are really a fold but Motile doesn't have arbitrary-arity folds
                      ;; so they're just expressed this way for now.
                      [frame-after-major-check
                       (cond 
                         [(and (not last-decoded-frame) (equal? replyaddr@ majorprox@))
                          ;; no prior frame. decode a new one and save it but only if 
                          ;; this frame is a header-carrying major frame.
                          ;; OK to try to decode (might not work this time if 
                          ;; this isn't a header-carrying frame)
                          (let* ([w/h (get-w/h m)]
                                 [this-frame-w (car w/h)]
                                 [this-frame-d (cdr w/h)])
                            (vp8dec-decode-copy decoder/major 
                                                (Frame.data v) 
                                                this-frame-w this-frame-d))]
                         ;; have prior frame and stream is major. update over prior frame
                         [(and last-decoded-frame (equal? replyaddr@ majorprox@))
                          (vp8dec-decode-update-major decoder/major 
                                                      (Frame.data v)
                                                      last-decoded-frame)]
                         ;; frame is minor stream only, or some other stream. ignore
                         [else last-decoded-frame])]
                      [frame-after-minor-check
                       (cond 
                         [(and frame-after-major-check (equal? replyaddr@ minorprox@))
                          ;; have prior frame and stream is minor. update over prior frame.
                          (vp8dec-decode-update-minor decoder/minor 
                                                      (Frame.data v)
                                                      frame-after-major-check)])]                    
                      ;; frame is major stream only, or some other stream. ignore
                      [else frame-after-major-check])
                 ;; if there is a frame to send out, send it down the pipeline
                 (when frame-after-minor-check
                   (ask/send* "POST" next-pipeline-element@
                              (Frame!data v frame-after-minor-check) (:message/ask/metadata r)))
                 ;; finished with this frame.
                 (loop decoder/major decoder/minor frame-after-minor-check
                       majorprox@ minorprox@ (thread-receive)))]
              ;; following are control messages passed from a controller backwards along the pipeline
              ;; to this decoder.
              [(and (InitiateBehavior? v) (eq? 'toggle-major/minor (InitiateBehavior.type v)))
               ; fixme: right now this deletes old states, makes fresh ones.
               ; with some tinkering they could be reused (there is a strange state issue to investigate)
               (map vp8dec-delete (list decoder/major decoder/minor))
               (loop (vp8dec-new) (vp8dec-new) #f minorprox@ majorprox@ (thread-receive))]
              [(and (InitiateBehavior? v) (eq? 'split (InitiateBehavior.type v)))
               ; to split just spawn two new fullscreen decoders.
               (map spawn-single-decoder (list majorprox@ minorprox@))
               (loop decoder/major decoder/minor last-decoded-frame majorprox@ minorprox@ (thread-receive))]
              [(GetParent? v)
               ; assumes parents are proxies
               (ask/send* "POST" (:message/ask/reply r) (cons majorprox@ minorprox@))
               (loop decoder/major decoder/minor last-decoded-frame majorprox@ minorprox@ (thread-receive))]
              [(CP? v)
               (respawn-self (CP.host v) (CP.port v))
               (loop decoder/major decoder/minor last-decoded-frame majorprox@ minorprox@ (thread-receive))]
              [(Quit/MV? v)
               (map remove-self-susbscription (list majorprox@ minorprox@))
               (map vp8dec-delete (list decoder/major decoder/minor))
               (respawn-self (Quit/MV.host v) (Quit/MV.port v))]
              [(Quit? v)
               (map remove-self-subscription (list majorprox@ minorprox@))
               (map vp8dec-delete (list decoder/major decoder/minor))]
              [else
               (printf "not a valid request to PIP decoder: ~a~n" v)
               (loop decoder/major decoder/minor last-decoded-frame majorprox@ minorprox@ (thread-receive))]
              )))))))

(define video-decoder/single
  (motile/compile
   '(lambda (reply@)
      (define (get-w/h frame)
        (let* ([metadata (:message/ask/metadata (cdr frame))]
               [params (cdr (assoc "params" metadata))])
          (cons (VideoParams.width params) (VideoParams.height params))))
      ;; 1. look up the current GUI, ask for a new display function
      (ask/send* "POST" (get-current-gui-curl) (AddCURL (current-curl)))
      (let ([gui-endpoint@ (car (thread-receive))]
            [d (vp8dec-new)])
        ;; 2. now that the decoder has a playback function it may decode frames, so ask for them.
        (ask/send* "POST" reply@ (AddCURL (current-curl)))
        ;; 3. start decoding loop
        (let loop ([m (thread-receive)])
          (define v (car m))
          (define r (cdr m))
          (cond [(Frame? v)
                 (let* ([w/h (get-w/h m)]
                        [this-frame-w (car w/h)]
                        [this-frame-d (cdr w/h)]
                        [decoded-frame (vp8dec-decode-copy d (Frame.data v) this-frame-w this-frame-d)])
                   (when decoded-frame
                     (ask/send* "POST" gui-endpoint@
                                (Frame!data v decoded-frame) (:message/ask/metadata r))))
                 (loop (thread-receive))]
                ;; following are messages send backwards across control flow path from controller.
                [(GetParent? v)
                 (ask/send* "POST" (:message/ask/reply r) reply@)
                 (loop (thread-receive))]
                ;; fix me: this is doing connector work when it is a component.
                [(FwdBackward? v)
                 (ask/send* "POST" reply@ (FwdBackward.msg v))
                 (loop (thread-receive))]
                [(CP? v)
                 (respawn-self (Quit/MV.host v) (Quit/MV.port v))
                 (loop (thread-receive))]
                [(Quit/MV? v)
                 (ask/send* "POST" reply@ (RemoveCURL (current-curl)))
                 (vp8dec-delete d)
                 (respawn-self (Quit/MV.host v) (Quit/MV.port v))]
                [(Quit? v)
                 (ask/send* "POST" reply@ (RemoveCURL (current-curl)))
                 (vp8dec-delete d)]
                [else
                 (printf "not a valid request to decoder: ~a~n" v)
                 (loop (thread-receive))]))))))

(define (video-reader/encoder devname w h)
  (motile/compile
   `(lambda (rpy@)
      (define default-fudge 0.5) ; used as a FACTOR of the overall framerate.
      ;; i.e., a default-fudge of 0.5 at a camera-specified framerate of 20 fps results in a
      ;; waiting period of (1000/20) * 0.5 = 25 ms between the first and second capture attempt.
      (define fudge-step 0.01)
      (define vreader (video-reader-setup ,devname ,w ,h))
      (let* ([params (video-reader-get-params vreader)]
             [framerate (bin/ (VideoParams.fpsNum params) (VideoParams.fpsDen params))])
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
          (define webm+params^ (make-metadata type/webm (cons "params" params)))
          (define (compress-full-frame e outbuff# fb)
            (define encoded# (vp8enc-encode e fb outbuff#))
            (when encoded# 
              (ask/send* "POST" target@ (FrameBuffer->Frame encoded#) webm+params^)))
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
          (let ([m (thread-receive)])
            (define v (car m))
            (define r (cdr m))
            (cond [(AddBehaviors? v)
                   ;; add new behaviors to the set of behaviors.
                   (k fudge (foldl (lambda (behavior-ctor cbs)
                                     (set/cons cbs (behavior-ctor params make-callback)))
                                   on-frame-callbacks
                                   (AddBehaviors.new-behaviors v)))]
                  [(Quit/MV? v)
                   (video-reader-delete vreader)
                   ;; clean up all the resources associated with a behavior (f #f)
                   (set/map on-frame-callbacks (lambda (f) (f #f)))
                   (respawn-self (Quit/MV.host v) (Quit/MV.port v))]
                  [else 
                   ;; unknown message
                   (k fudge on-frame-callbacks)])))
        ;; main loop.
        (let loop ([fudge default-fudge]
                   [on-frame-callbacks (set/cons set/equal/null (make-encode-full-frame-cb rpy@))])
          (if (thread-check-receive (bin* fudge framerate)) ;; <--- !! AIMD-based waiting here !!
              (do-control-message! loop fudge on-frame-callbacks)
              (do-one-frame!       loop fudge on-frame-callbacks)))))))

;; -----
;; AUDIO
;; -----

(define (audio-reader/speex-encoder echomod targeturl)
  (motile/compile
   `(let* ([enc (new-speex-encoder ,echomod)]
           [outbuff (make-bytes 1000)])
      (let loop ([ts (current-inexact-milliseconds)]
                 [available (speex-encoder-encode (vector-ref enc 0) outbuff)])
        (ask/send* "POST" ,targeturl 
                   (Frame (subbytes outbuff 0 available) ts)
                   (make-metadata type/speex))
        (loop (current-inexact-milliseconds)
              (speex-encoder-encode (vector-ref enc 0) outbuff))))))

(define (speex-decoder framesize)
  (motile/compile
   `(let ([d (new-speex-decoder ,framesize)])
      (let loop ([m (thread-receive)])
        (define v (car m))
        (cond [(Frame? v) 
               (speex-decoder-decode d (bytes-length (Frame.data v)) (Frame.data v))
               (loop (thread-receive))]
              [(Quit/MV? v)
               (speex-decoder-delete d)])))))
