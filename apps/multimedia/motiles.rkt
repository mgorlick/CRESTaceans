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
      
      (set-current-gui-curl! (current-curl))
      (let loop ([m (gui-thread-receive g)]
                 [decoders set/equal/null])
        (define v (car m))
        (cond [(message/uri? v)
               (displayln "GUI is adding a video")
               (let ([playback-function (let ([playback-canvas (box #f)]
                                              [av! video-gui-add-video!]
                                              [cp! bytes-copy!])
                                          (lambda (copyfunction w h)
                                            (unless (unbox playback-canvas)
                                              (box! playback-canvas (av! g w h v)))
                                            (copyfunction (unbox playback-canvas))))])
                 (ask/send* "POST" v playback-function #f)
                 (loop (gui-thread-receive g) (set/cons decoders v)))]
              
              [(CP? v)
               (displayln "GUI is copying")
               (respawn (current-curl) (CP.host v) (CP.port v))
               (set/map decoders (lambda (decoder) 
                                   (respawn decoder (CP.host v) (CP.port v))))
               (loop (gui-thread-receive g) decoders)]
              
              [(CP-child? v)
               (displayln "GUI is copying a decoder")
               (respawn (CP-child.curl v) (CP.host v) (CP.port v))
               (loop (gui-thread-receive g) decoders)]
              
              [(Quit/MV? v)
               (displayln "GUI is moving")
               (respawn (current-curl) (CP.host v) (CP.port v))
               (set/map decoders (lambda (decoder)
                                   (ask/send* "DELETE" decoder v #f)
                                   (respawn decoder (Quit/MV.host v) (Quit/MV.port v))))
               ;(shutdown-gui g)
               (clear-current-gui-curl!)]
              
              [(Quit? v)
               (displayln "GUI is quitting")
               (set/map decoders (lambda (decoder)
                                   (ask/send* "DELETE" decoder v #f)))
               ;(shutdown-gui g)
               (clear-current-gui-curl!)]
              
              [(gui-message-closed-feed? v)
               (printf "Feed of ~s was closed~n" (gui-message-closed-feed-curl v))
               (ask/send* "DELETE" (gui-message-closed-feed-curl v) (Quit) #f)
               (loop (gui-thread-receive g) 
                     (set/remove decoders (gui-message-closed-feed-curl v)))]
              [else
               (printf "Not a valid request to GUI: ~a~n" v)
               (loop (gui-thread-receive g) decoders)])))))

(define video-decoder/single
  (motile/compile
   '(lambda (reply-curl)
      (define (get-w/h frame)
        (let* ([metadata (:message/ask/metadata (cdr frame))]
               [params (cdr (assoc "params" metadata))])
          (cons (VideoParams.width params) (VideoParams.height params))))
      (define (copy-a-frame decoder frame w h sz buffer playback-function)
        (define (copy-from canvas)
          (vp8dec-decode-copy decoder (bytes-length (Frame.data frame)) (Frame.data frame) sz canvas))
        (playback-function copy-from w h))
      
      ;; 1. look up the current GUI, ask for a new display function
      (ask/send* "POST" (get-current-gui-curl) (current-curl) #f)
      (let ([playback-function (car (thread-receive))])
        ;; 2. now that the decoder has a playback function it may decode frames, so ask for them.
        (ask/send* "POST" reply-curl (AddCURL (current-curl)) #f)
        (let ([first (thread-receive)])
          ;; 3. need to set up the buffer with an appropriate size. pull the frame size from the metadata
          (cond [(Frame? (car first))
                 (let* ([w/h (get-w/h first)]
                        [w (car w/h)]
                        [h (cdr w/h)]
                        [sz (* 3 w h)]
                        [buffer (make-bytes sz)]
                        [d (vp8dec-new)])
                   ;; 4. start decoding loop
                   (let loop ([m first])
                     (define v (car m))
                     (define r (cdr m))
                     (cond [(Frame? v)
                            (copy-a-frame d v w h sz buffer playback-function)
                            (loop (thread-receive))]
                           
                           [(Quit/MV? v)
                            (displayln "Decoder is moving")
                            (ask/send* "POST" reply-curl (RemoveCURL (current-curl)) #f)
                            (vp8dec-delete d)]
                           
                           [(Quit? v)
                            (displayln "Decoder is quitting")
                            (ask/send* "POST" reply-curl (RemoveCURL (current-curl)) #f)
                            (vp8dec-delete d)]
                           
                           [(CP? v)
                            (displayln "Asked to CP - nothing to do")
                            (loop (thread-receive))]
                           
                           [else
                            (printf "not a valid request to decoder: ~a~n" v)
                            (loop (thread-receive))])))]
                [else (displayln "decoder protocol violation: message 2 should be frame #1")]))))))

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
