#lang racket/base

(require "../../Motile/compile/compile.rkt")
(provide (all-defined-out))

(define (big-bang encoder-location video-device video-w video-h decoder-location)
  (motile/compile
   `(lambda ()
      ; spawn the proxy
      (ask/send* "SPAWN" ,encoder-location (make-pubsubproxy) (make-metadata is/proxy))
      (let ([proxy-curl (car (thread-receive))])
        ; spawn the encoder with the proxy as reply addr
        (ask/send* "SPAWN" ,encoder-location
                   (make-video-reader/encoder ,video-device ,video-w ,video-h)
                   (make-metadata produces/webm) proxy-curl)
        ; spawn the decoder with the proxy as reply addr
        (ask/send* "SPAWN" ,decoder-location (make-single-decoder)
                   (make-metadata accepts/webm) proxy-curl)))))

(define pubsubproxy
  (motile/compile
   `(lambda ()
      (displayln "Proxy on")
      (let loop ([curls set/equal/null]
                 [last-sender-seen #f])
        (let ([m (thread-receive)])
          (define v (car m))
          (define r (cdr m))
          (cond 
            [(AddCURL? v)
             (loop (set/cons curls (AddCURL.curl v)) 
                   last-sender-seen)]
            
            [(RemoveCURL? v)
             (ask/send* "POST" (RemoveCURL.curl v) (Quit) (:message/ask/metadata r))
             (loop (set/remove curls (RemoveCURL.curl v)) 
                   last-sender-seen)]
            
            [(Frame? v)
             (set/map curls (lambda (crl) (ask/send* "POST" crl v (:message/ask/metadata r))))
             (loop curls 
                   (:message/ask/reply r))]
            
            [(AddBehaviors? v)
             (when last-sender-seen
               (ask/send* "POST" last-sender-seen v))
             (loop curls
                   last-sender-seen)]
            
            [(Quit/MV? v)
             (when last-sender-seen
               (ask/send* "POST" last-sender-seen v))
             (loop curls
                   last-sender-seen)]
            
            [else 
             (printf "proxy else: ~a~n" v)
             (loop curls
                   last-sender-seen)]))))))

;; -----
;; VIDEO
;; -----

(define gui-endpoint
  (motile/compile
   '(lambda (rpy buffer-maker!)      
      (define buffer& (box #f))
      (ask/send* "POST" rpy (current-curl))
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
               (loop (thread-receive))])))))

(define command-center-gui
  (motile/compile
   '(lambda (reply-curl)
      (define g (new-video-gui))
      
      (define (spawn-gui-endpoint curl)
        (ask/send* "SPAWN" (get-root-curl) (make-gui-endpoint)
                   (make-metadata 
                    is/endpoint
                    `("spawnargs" . ,(list curl
                                           (let ([add-video! video-gui-add-video!])
                                             ;; !!! NO SERIALIZATION WARNING !!!
                                             ;; (we lifted `g' and `add-video!' out of the lambda)
                                             ;; a grown-up implementation would check that this
                                             ;; curl `curl' is a local curl.
                                             (lambda (w h)
                                               (add-video! g w h curl))))))
                   curl))
      
      (set-current-gui-curl! (current-curl))
      
      (let loop ([m (thread-receive)]
                 [decoders set/equal/null])
        (define v (car m))
        (define r (cdr m))
        (cond 
          [(AddCURL? v)
           (displayln "GUI is adding a video")
           (spawn-gui-endpoint (AddCURL.curl v))
           (loop (thread-receive) (set/cons decoders (AddCURL.curl v)))]
          
          [(RemoveCURL? v)
           (printf "Feed of ~s was closed~n" (RemoveCURL.curl v))
           (ask/send* "DELETE" (RemoveCURL.curl v) (Quit))
           (loop (thread-receive) 
                 (set/remove decoders (RemoveCURL.curl v)))]
          
          [(PIPOn? v)
           (let ([the-pip ((make-pip-decoder) (list (PIPOn.major v) (PIPOn.minor v)))])
             (displayln "Making PIP")
             (ask/send* "SPAWN" (get-root-curl) the-pip
                        (make-metadata accepts/webm)
                        (get-root-curl)))
           (loop (thread-receive) decoders)]
          
          [(CP? v)
           (displayln "GUI is copying")
           (respawn-self (CP.host v) (CP.port v))
           (set/map decoders (lambda (decoder) 
                               (ask/send* "POST" decoder v)))
           (loop (thread-receive) decoders)]
          
          [(FwdBackward? v)
           (ask/send* "POST" (FwdBackward.ref v) v)
           (loop (thread-receive) decoders)]
          
          [(CP-child? v)
           (displayln "GUI is copying a decoder")
           (ask/send* "POST" (CP-child.curl v) (CP (CP-child.host v) (CP-child.port v)))
           (loop (thread-receive) decoders)]
          
          [(InitiateBehavior? v)
           (ask/send* "POST" (InitiateBehavior.ref v) v)
           (loop (thread-receive) decoders)]
          
          [(Quit/MV? v)
           (displayln "GUI is moving")
           (respawn-self (Quit/MV.host v) (Quit/MV.port v))
           (set/map decoders (lambda (decoder)
                               (ask/send* "DELETE" decoder v)))]
          
          [(Quit? v)
           (displayln "GUI is quitting")
           (set/map decoders (lambda (decoder)
                               (ask/send* "DELETE" decoder v)))]
          
          [else
           (printf "Not a valid request to GUI: ~a~n" v)
           (loop (thread-receive) decoders)])))))

(define video-decoder/pip
  (motile/compile
   '(lambda (decoder-curls)
      ;; do the decoder half of the decoder/GUI discovery protocol.
      ;; -> curl
      (define (retrieve-sink)
        (ask/send* "POST" (get-current-gui-curl) (AddCURL (current-curl)))
        (car (thread-receive)))
      ;; given the curl of a decoder ask for its proxy's curl.
      ;; curl -> curl
      (define (retrieve-proxy-from dcurl)
        (ask/send* "POST" dcurl (GetParent))
        (car (thread-receive)))
      ;; get the width/height of a message with a VideoParams as a metadata field (name `params')
      (define (get-w/h m)
        (let* ([metadata (:message/ask/metadata (cdr m))]
               [params (cdr (assoc "params" metadata))])
          (cons (VideoParams.width params) (VideoParams.height params))))
      ;; here's all the decoders we will use for the picture composition.
      (let ([proxy-curls (map retrieve-proxy-from decoder-curls)])
        (lambda ()
          (define gui-endpoint (retrieve-sink))
          (define majorpc (car proxy-curls))
          (define minorpc (cadr proxy-curls))
          ;; let the proxies know we're online...
          (ask/send* "POST" majorpc (AddCURL (current-curl)))
          (ask/send* "POST" minorpc (AddCURL (current-curl)))
          (let loop ([decoder/major (vp8dec-new)] ; vp8dec-pointer
                     [decoder/minor (vp8dec-new)] ; vp8dec-pointer            
                     [last-decoded-frame #f] ; or bytes #f
                     [curl/major majorpc] ; curl
                     [curl/minor minorpc] ; curl
                     [m (thread-receive)]) ; a message
            (define v (car m))
            (define r (cdr m))
            (cond 
              [(Frame? v)
               (let* ([replyaddr (:message/ask/reply r)]
                      ;; these two steps are really a fold but Motile doesn't have arbitrary-arity folds
                      ;; so they're just expressed this way for now.
                      [frame-after-major-check
                       (cond [(and (not last-decoded-frame)
                                   (equal? replyaddr curl/major))
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
                             [(and last-decoded-frame
                                   (equal? replyaddr curl/major))
                              (vp8dec-decode-update-major decoder/major 
                                                          (Frame.data v)
                                                          last-decoded-frame)]
                             ;; frame is minor stream only, or some other stream. ignore
                             [else last-decoded-frame])]
                      [frame-after-minor-check
                       (cond [(and frame-after-major-check
                                   (equal? replyaddr curl/minor))
                              ;; have prior frame and stream is minor. update over prior frame.
                              (vp8dec-decode-update-minor decoder/minor 
                                                          (Frame.data v)
                                                          frame-after-major-check)])]                    
                      ;; frame is major stream only, or some other stream. ignore
                      [else frame-after-major-check])
                 
                 (when frame-after-minor-check
                   (ask/send* "POST" gui-endpoint
                              (Frame!data v frame-after-minor-check) (:message/ask/metadata r)))
                 
                 (loop decoder/major decoder/minor frame-after-minor-check
                       curl/major curl/minor (thread-receive)))]
              
              [(and (InitiateBehavior? v)
                    (eq? 'toggle-major/minor (InitiateBehavior.type v)))
               (displayln "Swapping")
               (vp8dec-delete decoder/major)
               (vp8dec-delete decoder/minor)
               (loop (vp8dec-new) (vp8dec-new) #f
                     curl/minor curl/major (thread-receive))]
              
              [(and (InitiateBehavior? v)
                    (eq? 'split (InitiateBehavior.type v)))
               (displayln "Splitting")
               (ask/send* "SPAWN" (get-root-curl) (make-single-decoder)
                          (make-metadata accepts/webm) curl/major)
               (ask/send* "SPAWN" (get-root-curl) (make-single-decoder)
                          (make-metadata accepts/webm) curl/minor)
               (loop decoder/major decoder/minor last-decoded-frame
                     curl/major curl/minor (thread-receive))]
              
              [(GetParent? v)
               (ask/send* "POST" (:message/ask/reply r) (cons curl/major curl/minor))
               (loop decoder/major decoder/minor last-decoded-frame
                     curl/major curl/minor (thread-receive))]
              
              [(CP? v)
               (respawn-self (CP.host v) (CP.port v))
               (loop decoder/major decoder/minor last-decoded-frame 
                     curl/major curl/minor (thread-receive))]
              
              [(Quit/MV? v)
               (displayln "PIP Decoder is moving")
               (ask/send* "POST" curl/major (RemoveCURL (current-curl)))
               (ask/send* "POST" curl/minor (RemoveCURL (current-curl)))
               (vp8dec-delete decoder/major)
               (vp8dec-delete decoder/minor)
               (respawn-self (Quit/MV.host v) (Quit/MV.port v))]
              
              [(Quit? v)
               (displayln "PIP Decoder is quitting")
               (ask/send* "POST" curl/major (RemoveCURL (current-curl)))
               (ask/send* "POST" curl/minor (RemoveCURL (current-curl)))
               (vp8dec-delete decoder/major)
               (vp8dec-delete decoder/minor)]
              
              [else
               (printf "not a valid request to PIP decoder: ~a~n" v)
               (loop decoder/major decoder/minor last-decoded-frame
                     curl/major curl/minor (thread-receive))])))))))

(define video-decoder/single
  (motile/compile
   '(lambda (reply-curl)
      (define (get-w/h frame)
        (let* ([metadata (:message/ask/metadata (cdr frame))]
               [params (cdr (assoc "params" metadata))])
          (cons (VideoParams.width params) (VideoParams.height params))))
      ;; 1. look up the current GUI, ask for a new display function
      (ask/send* "POST" (get-current-gui-curl) (AddCURL (current-curl)))
      (let ([gui-endpoint<~> (car (thread-receive))]
            [d (vp8dec-new)])
        ;; 2. now that the decoder has a playback function it may decode frames, so ask for them.
        (ask/send* "POST" reply-curl (AddCURL (current-curl)))
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
                     (ask/send* "POST" gui-endpoint<~>
                                (Frame!data v decoded-frame) (:message/ask/metadata r))))
                 (loop (thread-receive))]
                
                [(GetParent? v)
                 (ask/send* "POST" (:message/ask/reply r) reply-curl)
                 (loop (thread-receive))]
                
                [(FwdBackward? v)
                 (ask/send* "POST" reply-curl (FwdBackward.msg v))
                 (loop (thread-receive))]
                
                [(CP? v)
                 (displayln "Decoder is copying proxy CURL")
                 (respawn-self (Quit/MV.host v) (Quit/MV.port v))
                 (loop (thread-receive))]
                
                [(Quit/MV? v)
                 (displayln "Decoder is quitting")
                 (ask/send* "POST" reply-curl (RemoveCURL (current-curl)))
                 (vp8dec-delete d)
                 (respawn-self (Quit/MV.host v) (Quit/MV.port v))]
                
                [(Quit? v)
                 (displayln "Decoder is quitting")
                 (ask/send* "POST" reply-curl (RemoveCURL (current-curl)))
                 (vp8dec-delete d)]
                
                [else
                 (printf "not a valid request to decoder: ~a~n" v)
                 (loop (thread-receive))]))))))

(define (video-reader/encoder devname w h)
  (motile/compile
   `(lambda (proxy-curl)
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
          (define ctor (if (equal? type 'full)
                           vp8enc-new
                           vp8enc-quartersize-new))
          (let ([e (ctor params)]
                [outbuff (make-bytes (bin* 1024 256))])
            (lambda (fb)
              (if fb
                  (f e outbuff fb)
                  (vp8enc-delete e)))))
        
        ; the default callback: a full-frame encoding.
        (define (make-encode-full-frame-cb target)
          (define meta-to-use
            (make-metadata type/webm
                           (cons "params" params)))
          (make-callback 'full
                         (lambda (e outbuff fb)
                           (define encoded (vp8enc-encode e fb outbuff))
                           (and encoded (ask/send* "POST" target (FrameBuffer->Frame encoded) 
                                                   meta-to-use)))))
        
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
        
        (define (do-control-message! k fudge on-frame-callbacks)
          (let ([m (thread-receive)])
            (define v (car m))
            (define r (cdr m))
            (cond [(AddBehaviors? v)
                   (k fudge (foldl (lambda (behavior-ctor cbs)
                                     (set/cons cbs (behavior-ctor params make-callback)))
                                   on-frame-callbacks
                                   (AddBehaviors.new-behaviors v)))]
                  [(Quit/MV? v)
                   (video-reader-delete vreader)
                   (set/map on-frame-callbacks (lambda (f) (f #f)))
                   (respawn-self (Quit/MV.host v) (Quit/MV.port v))]
                  [else (k fudge on-frame-callbacks)])))
        
        (let loop ([fudge default-fudge]
                   [on-frame-callbacks (set/cons set/equal/null 
                                                 (make-encode-full-frame-cb proxy-curl))])
          (if (thread-check-receive (bin* fudge framerate))
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
