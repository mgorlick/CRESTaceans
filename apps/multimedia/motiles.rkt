#lang racket/base

(require "../../Motile/compile/compile.rkt")
(provide (all-defined-out))

(define relayer
  (motile/compile
   `(let relay ([curls set/equal/null]
                [m (thread-receive)])
      (define v (car m))
      (define r (cdr m))
      (cond [(AddCURL? v)
             (printf "subscribing new consumer: ~a~n" (AddCURL.curl v))
             (relay (set/cons curls (AddCURL.curl v))
                    (thread-receive))]
            
            [(RemoveCURL? v)
             (ask/send* "POST" (RemoveCURL.curl v) (Quit) (:message/ask/metadata r))
             (relay (set/remove curls (RemoveCURL.curl v))
                    (thread-receive))]
            
            [(Frame? v)
             (set/map curls (lambda (crl) (ask/send* "POST" crl v (:message/ask/metadata r))))
             (relay curls (thread-receive))]
            
            [else (printf "relay else: ~a~n" v)]))))

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
            (unless (unbox playback-canvas) (box! playback-canvas (av! g w h curl)))
            (copyfunction (bytes-length (unbox playback-canvas)) (unbox playback-canvas)))))
      
      (define (retrieve-proxy-from-decoder dcurl)
        (ask/send* "POST" dcurl (CP #f #f) #f)
        (car (thread-receive)))
      
      (set-current-gui-curl! (current-curl))
      (let loop ([m (thread-receive)]
                 [decoders set/equal/null])
        (define v (car m))
        (define r (cdr m))
        (cond [(AddCURL? v)
               (displayln "GUI is adding a video")
               (ask/send* "POST" (:message/ask/reply r) (add-new (AddCURL.curl v)) #f)
               (loop (thread-receive) (set/cons decoders (AddCURL.curl v)))]
              
              [(CP? v)
               (displayln "GUI is copying")
               (respawn (current-curl) (CP.host v) (CP.port v))
               (set/map decoders (lambda (decoder) 
                                   (respawn decoder (CP.host v) (CP.port v))))
               (loop (thread-receive) decoders)]
              
              [(CP-child? v)
               (displayln "GUI is copying a decoder")
               (respawn (CP-child.curl v) (CP.host v) (CP.port v))
               (loop (thread-receive) decoders)]
              
              [(Quit/MV? v)
               (displayln "GUI is moving")
               (respawn (current-curl) (CP.host v) (CP.port v))
               (set/map decoders (lambda (decoder)
                                   (ask/send* "DELETE" decoder v #f)
                                   (respawn decoder (Quit/MV.host v) (Quit/MV.port v))))]
              
              [(Quit? v)
               (displayln "GUI is quitting")
               (set/map decoders (lambda (decoder)
                                   (ask/send* "DELETE" decoder v #f)))]
              
              [(RemoveCURL? v)
               (printf "Feed of ~s was closed~n" (RemoveCURL.curl v))
               (ask/send* "DELETE" (RemoveCURL.curl v) (Quit) #f)
               (loop (thread-receive) 
                     (set/remove decoders (RemoveCURL.curl v)))]
              
              [(PIPOn? v)
               (let ([new-pip-decoder ((pip) (retrieve-proxy-from-decoder (PIPOn.major v))
                                             (retrieve-proxy-from-decoder (PIPOn.minor v)))])
                 (printf "New pip: ~a~n" new-pip-decoder)
                 (ask/send* "SPAWN" root-curl new-pip-decoder (metadata accepts/webm) root-curl))
               (loop (thread-receive) decoders)]
              
              [else
               (printf "Not a valid request to GUI: ~a~n" m)
               (loop (thread-receive) decoders)])))))

(define video-decoder/pip
  (motile/compile
   '(lambda (majorpc minorpc)
      (define (retrieve-playback-function)
        (ask/send* "POST" (get-current-gui-curl) (AddCURL (current-curl)) #f)
        (car (thread-receive)))
      (lambda ()
        (define playback-function (retrieve-playback-function))
        (define decoder/major (vp8dec-new))
        (define decoder/minor (vp8dec-new))
        ;; let the proxies know we're online...
        (ask/send* "POST" majorpc (AddCURL (current-curl)) #f)
        (ask/send* "POST" minorpc (AddCURL (current-curl)) #f)
        (let ()
          (define (get-w/h m)
            (let* ([metadata (:message/ask/metadata (cdr m))]
                   [params (cdr (assoc "params" metadata))])
              (cons (VideoParams.width params) (VideoParams.height params))))
          (define (update updater decoder w/h data)
            (playback-function (car w/h) (cdr w/h)
                               (lambda (sz canvas)
                                 (updater decoder (bytes-length data) data sz canvas))))
          (let loop ([frames hash/equal/null] [m (thread-receive)])
            (define v (car m))
            (define r (cdr m))
            (cond [(Frame? v)
                   (let* ([replyaddr (:message/ask/reply r)]
                          [w/h (get-w/h m)])
                     (cond 
                       [(equal? replyaddr majorpc)
                        (update (if (hash/contains? frames minorpc)
                                    vp8dec-decode-update-major
                                    vp8dec-decode-copy)
                                decoder/major w/h (Frame.data v))]
                       [(equal? replyaddr minorpc)
                        (update vp8dec-decode-update-minor decoder/minor w/h (Frame.data v))])
                     (loop 
                      ;; throw the new frame away if it's not one of the streams we're interested in
                      ;; at the current time, or merge it with the old set of frames if we are.
                      (cond [(or (equal? replyaddr majorpc) (equal? replyaddr minorpc))
                             (hash/cons frames replyaddr v)]
                            [else
                             frames])
                      (thread-receive)))]
                  
                  [(or (Quit/MV? v) (Quit? v))
                   (displayln "PIP Decoder is quitting")
                   (ask/send* "POST" majorpc (RemoveCURL (current-curl)) #f)
                   (ask/send* "POST" minorpc (RemoveCURL (current-curl)) #f)
                   (vp8dec-delete decoder/major)
                   (vp8dec-delete decoder/minor)]
                  
                  [else
                   (printf "not a valid request to PIP decoder: ~a~n" v)
                   (loop frames (thread-receive))])))))))

(define video-decoder/single
  (motile/compile
   '(lambda (reply-curl)
      
      (define (get-w/h frame)
        (let* ([metadata (:message/ask/metadata (cdr frame))]
               [params (cdr (assoc "params" metadata))])
          (cons (VideoParams.width params) (VideoParams.height params))))
      
      ;; 1. look up the current GUI, ask for a new display function
      (ask/send* "POST" (get-current-gui-curl) (AddCURL (current-curl)) #f)
      (let ([playback-function (car (thread-receive))]
            [d (vp8dec-new)])
        ;; 2. now that the decoder has a playback function it may decode frames, so ask for them.
        (ask/send* "POST" reply-curl (AddCURL (current-curl)) #f)
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
                
                [(CP? v)
                 (displayln "Decoder is copying proxy CURL")
                 (ask/send* "POST" (:message/ask/reply r) reply-curl #f)
                 (loop (thread-receive))]
                
                [(or (Quit/MV? v) (Quit? v))
                 (displayln "Decoder is moving")
                 (ask/send* "POST" reply-curl (RemoveCURL (current-curl)) #f)
                 (vp8dec-delete d)]
                
                [else
                 (printf "not a valid request to decoder: ~a~n" v)
                 (loop (thread-receive))]))))))

(define (video-reader/encoder devname w h)
  (motile/compile
   `(lambda (relayer-curl)
      (define default-fudge 0.5) ; used as a FACTOR of the overall framerate.
      ;; i.e., a default-fudge of 0.5 at a camera-specified framerate of 20 fps results in a
      ;; waiting period of (1000/20) * 0.5 = 25 ms between the first and second capture attempt.
      (define fudge-step 0.01)
      (define outbuff (make-bytes (bin* 1024 256))) ; used for encoding only, not capture.
      (define v (video-reader-setup ,devname ,w ,h))
      (let* ([params (video-reader-get-params v)]
             [e (vp8enc-new params)]
             [framerate (bin/ (VideoParams.fpsNum params) (VideoParams.fpsDen params))])
        
        ; grab-frame: int -> or FrameBuffer #f
        (define (grab-frame ts)
          (cond [(video-reader-is-ready? v) (video-reader-get-frame v ts)]
                [else #f]))
        
        ; encode-frame: or FrameBuffer #f -> or FrameBuffer #f
        (define (encode-frame frame)
          (cond [frame (vp8enc-encode/return-frame e frame outbuff)]
                [else frame]))
        
        ; grab/encode: -> or FrameBuffer #f
        (define (grab/encode)
          (encode-frame (grab-frame (current-inexact-milliseconds))))
        
        ; loop: featuring AIMD waiting for camera frames.
        ; 1. try to read the next frame.
        ; 2a. if the frame was successfully read and encoded, send it off to the proxy curl.
        ;     then, loop with an additive decrease in waiting time till the next frame.
        ; 2b. if the frame was NOT successfully read and encoded, multiplicatively increase
        ;     the waiting time til the next frame. 
        ;     this seems to give relatively constant framerate overall, with few instances of
        ;     many "misses" in a row, where a "miss" is checking the camera before it is ready.
        (let loop ([fudge default-fudge])
          (define v (grab/encode))
          (cond [v
                 ; Frame received successfully. Pass it on and then wait until
                 ; the next frame should be active (modified by a factor to account for processing time)
                 ;(printf "HIT: fudge ~a~n" fudge)
                 (ask/send* "POST" relayer-curl (FrameBuffer->Frame v) 
                            (metadata type/webm (cons "params" params)))
                 (sleep* (bin* fudge framerate))
                 ; decrease fudge factor for next frame a la AIMD.
                 (loop (if (bin>= fudge fudge-step)
                           (bin- fudge fudge-step)
                           0))]
                [else
                 ;(printf "MISS: fudge ~a~n" fudge)
                 ; increase fudge factor for next frame a la AIMD.
                 (loop (min* default-fudge (bin* 2 (max* fudge-step fudge))))]))))))

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
                   (make-metadta type/speex))
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
