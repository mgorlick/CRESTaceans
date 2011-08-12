#lang racket/base

(require "../../Motile/compile.rkt")
(provide (all-defined-out))

(define relayer
  (motile/compile
   `(let relay ([curls (hash/new hash/equal/null)]
                [m (thread-receive)])
      (let ([v (car m)]
            [r (cdr m)])
        (cond [(AddCURL? v)
               (relay (hash/cons curls (AddCURL.curl v) (:message/ask/metadata r))
                      (thread-receive))]
              
              [(RemoveCURL? v)
               (ask/send* "POST" (RemoveCURL.curl v) (Quit) (:message/ask/metadata r))
               (relay (hash/remove curls (RemoveCURL.curl v))
                      (thread-receive))]
              
              [(Frame? v)
               (let sendloop ([c (hash/keys curls)])
                 (cond [(not (null? c)) (ask/send* "POST" (car c) v (:message/ask/metadata r))
                                        (sendloop (cdr c))]))
               (relay curls (thread-receive))]
              
              [else (printf "relay else: ~a~n" v)])))))

;; -----
;; VIDEO
;; -----

(define command-center-gui
  (motile/compile
   `(lambda (reply-curl)
      (set-current-gui-curl! (current-curl))
      (let ([g (new-video-gui 320 240)])
        (let loop ([m (gui-thread-receive g)] [decoders (hash/new hash/equal/null)])
          (let ([v (car m)])
            (cond [(AddDecodedVideo? v)
                   (displayln "GUI is adding a video")
                   (let ([playback 
                          (video-gui-add-video! g (AddDecodedVideo.w v) (AddDecodedVideo.h v)
                                                (AddDecodedVideo.decodercurl v))])
                     (ask/send* "POST" (AddDecodedVideo.decodercurl v) playback #f)
                     (loop (gui-thread-receive g)
                           (hash/cons decoders (AddDecodedVideo.decodercurl v) #f)))]
                  
                  [(CP? v)
                   (displayln "GUI is copying")
                   (respawn (current-curl) (CP.host v) (CP.port v))
                   (for-each (lambda (decoder)
                               (respawn decoder (CP.host v) (CP.port v)))
                             (hash/keys decoders))
                   (loop (gui-thread-receive g) decoders)]
                  
                  [(CP-child? v)
                   (displayln "GUI is copying a decoder")
                   (respawn (CP-child.curl v) (CP.host v) (CP.port v))
                   (loop (gui-thread-receive g) decoders)]
                  
                  [(Quit/MV? v)
                   (displayln "GUI is moving")
                   (respawn (current-curl) (CP.host v) (CP.port v))
                   (for-each (lambda (decoder)
                               (ask/send* "DELETE" decoder v #f)
                               (respawn decoder (Quit/MV.host v) (Quit/MV.port v)))
                             (hash/keys decoders))
                   (shutdown-gui g)
                   (clear-current-gui-curl!)]
                  
                  [(Quit? v)
                   (displayln "GUI is quitting")
                   (for-each (lambda (decoder)
                               (ask/send* "DELETE" decoder v #f))
                             (hash/keys decoders))
                   (shutdown-gui g)
                   (clear-current-gui-curl!)]
                  
                  [(gui-message-closed-feed? v)
                   (printf "Feed of ~s was closed~n" (gui-message-closed-feed-curl v))
                   (ask/send* "DELETE" (gui-message-closed-feed-curl v) (Quit) #f)
                   (loop (gui-thread-receive g) 
                         (hash/remove decoders (gui-message-closed-feed-curl v)))]
                  
                  [else
                   (printf "Not a valid request to GUI: ~a~n" v)
                   (loop (gui-thread-receive g) decoders)])))))))

(define (video-decoder/gui w h)
  (motile/compile
   `(lambda (reply-curl)
      (ask/send* "POST" (get-current-gui-curl) (AddDecodedVideo ,w ,h (current-curl)) #f)
      (let* ([d (vp8dec-new)]
             [playback (car (thread-receive))]
             [sz (video-playback-buffersize playback)]
             [buffer (video-playback-buffer playback)]
             [_ (ask/send* "POST" reply-curl (AddCURL (current-curl)) #f)])
        (let loop ([m (thread-receive)])
          (let* ([v (car m)] [r (cdr m)])
            (cond [(Frame? v)
                   (vp8dec-decode-copy d (bytes-length (Frame.data v)) (Frame.data v) sz buffer)
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
                   (loop (thread-receive))])))))))

(define (video-reader/encoder devname w h)
  (motile/compile
   `(lambda (relayer-curl)
      (define default-fudge 0.5)
      (define fudge-step 0.01)
      (let* ([v (video-reader-setup ,devname ,w ,h)]
             [params (video-reader-get-params v)]
             [e (vp8enc-new params)]
             [buffsize (bin* 1024 256)]
             [outbuff (make-bytes buffsize)]
             [framerate (bin/ (VideoParams.fpsNum params) (VideoParams.fpsDen params))])
        ; grab-frame: int -> or FrameBuffer None
        (define (grab-frame ts)
          (cond [(video-reader-is-ready? v) (video-reader-get-frame v ts)]
                [else #f]))
        ; encode-frame: or FrameBuffer None -> or FrameBuffer None
        (define (encode-frame frame)
          (cond [frame (vp8enc-encode/return-frame e frame outbuff)]
                [else frame]))
        ; grab/encode: -> or FrameBuffer None
        (define (grab/encode)
          (encode-frame (grab-frame (current-inexact-milliseconds))))
        (let loop ([v (grab/encode)] [fudge default-fudge])
          (cond [v
                 ; Frame received successfully. Pass it on and then wait until
                 ; the next frame should be active (modified by a factor to account for processing time)
                 ;(printf "HIT: fudge ~a~n" fudge)
                 (ask/send* "POST" relayer-curl (FrameBuffer->Frame v) (metadata type/webm (cons "params" params)))
                 (sleep* (bin* fudge framerate))
                 ; decrease fudge factor for next frame a la AIMD.
                 (loop (grab/encode) (if (bin>= fudge fudge-step)
                                         (bin- fudge fudge-step)
                                         0))]
                [else
                 ;(printf "MISS: fudge ~a~n" fudge)
                 (loop (grab/encode)
                       ; increase fudge factor for next frame a la AIMD.
                       (min* default-fudge (bin* 2 (max* fudge-step fudge))))]))))))

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