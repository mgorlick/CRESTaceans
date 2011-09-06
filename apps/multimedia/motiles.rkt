#lang racket/base

(require "../../Motile/compile/compile.rkt")
(provide (all-defined-out))

(define (big-bang encoder-location video-device video-w video-h decoder-location)
  (motile/compile
   `(lambda ()
      (displayln "Big bang on")
      ; spawn the proxy
      (ask/send* "SPAWN" ,encoder-location (make-pubsubproxy) (make-metadata is/proxy))
      (displayln "Proxy spawned")
      (let ([proxy-curl (car (thread-receive))])
        ; spawn the encoder with the proxy as reply addr
        (displayln "Spawning encoder")
        (ask/send* "SPAWN" ,encoder-location
                   (make-encoder ,video-device ,video-w ,video-h)
                   (make-metadata produces/webm) proxy-curl)
        ; spawn the decoder with the proxy as reply addr
        (displayln "Spawning decoder")
        (ask/send* "SPAWN" ,decoder-location (make-single-decoder) (make-metadata accepts/webm) proxy-curl)))))

(define pubsubproxy
  (motile/compile
   `(lambda (rpy)
      (displayln "Proxy on")
      (let loop ([curls set/equal/null]
                 [m (thread-receive)])
        (define v (car m))
        (define r (cdr m))
        (cond [(AddCURL? v)
               (loop (set/cons curls (AddCURL.curl v)) (thread-receive))]
              
              [(RemoveCURL? v)
               (ask/send* "POST" (RemoveCURL.curl v) (Quit) (:message/ask/metadata r))
               (loop (set/remove curls (RemoveCURL.curl v)) (thread-receive))]
              
              [(Frame? v)
               (set/map curls (lambda (crl) (ask/send* "POST" crl v (:message/ask/metadata r))))
               (loop curls (thread-receive))]
              
              [else (printf "proxy else: ~a~n" v)
                    (loop curls (thread-receive))])))))

;; -----
;; VIDEO
;; -----

(define command-center-gui
  (motile/compile
   `(lambda (reply-curl)
      (define g (new-video-gui 320 240))
      
      (define (add-new curl)
        (let ([playback-canvas (box #f)]
              [av! video-gui-add-video!])
          (lambda (w h copyfunction)
            (unless (unbox playback-canvas) 
              (box! playback-canvas (av! g w h curl)))
            (copyfunction (bytes-length (unbox playback-canvas)) (unbox playback-canvas)))))
      
      (set-current-gui-curl! (current-curl))
      (let loop ([m (thread-receive)]
                 [decoders set/equal/null])
        (define v (car m))
        (define r (cdr m))
        (cond [(AddCURL? v)
               (displayln "GUI is adding a video")
               (ask/send* "POST" (:message/ask/reply r) (add-new (AddCURL.curl v)))
               (loop (thread-receive) (set/cons decoders (AddCURL.curl v)))]
              
              [(RemoveCURL? v)
               (printf "Feed of ~s was closed~n" (RemoveCURL.curl v))
               (ask/send* "DELETE" (RemoveCURL.curl v) (Quit))
               (loop (thread-receive) 
                     (set/remove decoders (RemoveCURL.curl v)))]
              
              [(PIPOn? v)
               (let ([the-pip ((make-pip-decoder) (list (PIPOn.major v) (PIPOn.minor v)))])
                 (displayln the-pip)
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
              
              [(CP-child? v)
               (displayln "GUI is copying a decoder")
               (ask/send* "POST" (CP-child.curl v) (CP (CP-child.host v)
                                                       (CP-child.port v)))
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
      (define (retrieve-playback-function)
        (ask/send* "POST" (get-current-gui-curl) (AddCURL (current-curl)))
        (car (thread-receive)))
      (define (retrieve-proxy-from dcurl)
        (ask/send* "POST" dcurl (GetParent))
        (car (thread-receive)))
      (define (get-w/h m)
        (let* ([metadata (:message/ask/metadata (cdr m))]
               [params (cdr (assoc "params" metadata))])
          (cons (VideoParams.width params) (VideoParams.height params))))
      (displayln "Generating PIP lambda")
      (displayln "Proxies:")
      (let ([proxy-curls (map retrieve-proxy-from decoder-curls)])
        (displayln proxy-curls)
        (lambda ()
          (define playback-function (retrieve-playback-function))
          (define majorpc (car proxy-curls))
          (define minorpc (cadr proxy-curls))
          (define decoder/major (vp8dec-new))
          (define decoder/minor (vp8dec-new))
          ;; let the proxies know we're online...
          (ask/send* "POST" majorpc (AddCURL (current-curl)))
          (ask/send* "POST" minorpc (AddCURL (current-curl)))
          (let ()
            (define (update updater decoder w/h data)
              (playback-function (car w/h) (cdr w/h)
                                 (lambda (sz canvas) (updater decoder (bytes-length data) data sz canvas))))
            (let loop ([frames hash/equal/null] [m (thread-receive)])
              (define v (car m))
              (define r (cdr m))
              (cond [(Frame? v)
                     (let ([replyaddr (:message/ask/reply r)]
                           [w/h (get-w/h m)])
                       (when (equal? replyaddr majorpc)
                         (update (if (hash/contains? frames minorpc)
                                     vp8dec-decode-update-major
                                     vp8dec-decode-copy)
                                 decoder/major w/h (Frame.data v)))
                       (when (equal? replyaddr minorpc)
                         (update vp8dec-decode-update-minor decoder/minor w/h (Frame.data v)))
                       (loop 
                        ;; throw the new frame away if it's not one of the streams we're interested in
                        ;; at the current time, or merge it with the old set of frames if we are.
                        (cond [(or (equal? replyaddr majorpc) (equal? replyaddr minorpc))
                               (hash/cons frames replyaddr v)]
                              [else
                               frames])
                        (thread-receive)))]
                    
                    [(GetParent? v)
                     (ask/send* "POST" (:message/ask/reply r) (cons majorpc minorpc))
                     (loop frames (thread-receive))]
                    
                    [(CP? v)
                     (respawn-self (CP.host v) (CP.port v))
                     (loop frames (thread-receive))]
                    
                    [(Quit/MV? v)
                     (displayln "PIP Decoder is moving")
                     (ask/send* "POST" majorpc (RemoveCURL (current-curl)))
                     (ask/send* "POST" minorpc (RemoveCURL (current-curl)))
                     (vp8dec-delete decoder/major)
                     (vp8dec-delete decoder/minor)
                     (respawn-self (Quit/MV.host v) (Quit/MV.port v))]
                    
                    [(Quit? v)
                     (displayln "PIP Decoder is quitting")
                     (ask/send* "POST" majorpc (RemoveCURL (current-curl)))
                     (ask/send* "POST" minorpc (RemoveCURL (current-curl)))
                     (vp8dec-delete decoder/major)
                     (vp8dec-delete decoder/minor)]
                    
                    [else
                     (printf "not a valid request to PIP decoder: ~a~n" v)
                     (loop frames (thread-receive))]))))))))

(define video-decoder/single
  (motile/compile
   '(lambda (reply-curl)
      
      (define (get-w/h frame)
        (let* ([metadata (:message/ask/metadata (cdr frame))]
               [params (cdr (assoc "params" metadata))])
          (cons (VideoParams.width params) (VideoParams.height params))))
      
      ;; 1. look up the current GUI, ask for a new display function
      (ask/send* "POST" (get-current-gui-curl) (AddCURL (current-curl)))
      (let ([playback-function (car (thread-receive))]
            [d (vp8dec-new)])
        ;; 2. now that the decoder has a playback function it may decode frames, so ask for them.
        (ask/send* "POST" reply-curl (AddCURL (current-curl)))
        ;; 3. start decoding loop
        (let loop ([m (thread-receive)])
          (define v (car m))
          (define r (cdr m))
          (cond [(Frame? v)
                 (let ([w/h (get-w/h m)]
                       [data (Frame.data v)])
                   (playback-function (car w/h) (cdr w/h)
                                      (lambda (sz canvas)
                                        (vp8dec-decode-copy d (bytes-length data) data sz canvas))))
                 (loop (thread-receive))]
                
                [(GetParent? v)
                 (ask/send* "POST" (:message/ask/reply r) reply-curl)
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
          (make-callback 'full
                         (lambda (e outbuff fb)
                           (define encoded (vp8enc-encode e fb outbuff))
                           (and encoded (ask/send* "POST" target (FrameBuffer->Frame encoded) 
                                                   (make-metadata type/webm (cons "params" params)))))))
        
        
        ; additional callbacks can be created to make quarter-frame tiles.
        (define (make-encode-quarter-frame-cb target row col)
          (define halved-params
            (VideoParams!width (VideoParams!height params (/ (VideoParams.height params) 2))
                               (/ (VideoParams.width params) 2)))
          (make-callback 'quarter
                         (lambda (e outbuff fb)
                           (define encoded (vp8enc-encode-quarter e fb outbuff row col))
                           (and encoded (ask/send* "POST" target (FrameBuffer->Frame encoded) 
                                                   (make-metadata type/webm (cons "params" halved-params)))))))
        
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
            (cond [(Quit/MV? v)
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
