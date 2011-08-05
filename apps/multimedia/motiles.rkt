#lang racket/base

(require "../../Motile/compile.rkt")
(provide (all-defined-out))

(define (relayer metadata)
  (motile/compile
   `(let relay ([curls (hash/new hash/equal/null)]
                [v (thread-receive)])
      
      (cond [(AddCURL? v)
             (relay (hash/cons curls (AddCURL.curl v) #f)
                    (thread-receive))]
            
            [(RemoveCURL? v)
             (ask/send* "POST" (RemoveCURL.curl v) (Quit) #f)
             (relay (hash/remove curls (RemoveCURL.curl v))
                    (thread-receive))]
            
            [(Frame? v)
             (let sendloop ([c (hash/keys curls)])
               (cond [(not (null? c)) (ask/send* "POST" (car c) v #f)
                                      (sendloop (cdr c))]))
             (relay curls (thread-receive))]
            
            [else (printf "relay else: ~a~n" v)]))))

;; -----
;; VIDEO
;; -----

(define command-center-gui
  (motile/compile
   '(lambda (my-curl reply-curl)
      (let ([g (new-video-gui 640 480)])
        (ask/send* "POST" reply-curl my-curl #f)
        (let loop ([v (thread-receive)])
          (cond [(AddDecodedVideo? v)
                 (let ([playback (video-gui-add-video! g (AddDecodedVideo.w v) (AddDecodedVideo.h v) 
                                                       (message/uri->string (AddDecodedVideo.decodercurl v)))])
                   (ask/send* "POST" (AddDecodedVideo.decodercurl v) playback #f)
                   (loop (thread-receive)))]
                [else
                 (printf "Not a valid request to GUI: ~a~n" v)
                 (loop (thread-receive))]))))))

(define (video-decoder/gui gui-curl)
  (motile/compile
   `(lambda (my-curl reply-curl)
      (ask/send* "POST" ,gui-curl (AddDecodedVideo 640 480 my-curl) #f)
      (let* ([d (vp8dec-new)]
             [playback (thread-receive)]
             [sz (video-playback-buffersize playback)]
             [buffer (video-playback-buffer playback)]
             [__ignore (ask/send* "POST" reply-curl (AddCURL my-curl) #f)])
        (let loop ([v (thread-receive)])
          (cond [(Frame? v)      
                 (vp8dec-decode-copy d (bytes-length (Frame.data v)) (Frame.data v) sz buffer)
                 (loop (thread-receive))]
                [(Quit? v)
                 (vp8dec-delete d)]
                [else
                 (printf "not a message: ~a~n" v)]))))))

(define video-reader/encoder
  (motile/compile
   '(lambda (relayer-curl)
      (define default-fudge 0.5)
      (define fudge-step 0.01)
      (let* ([v (video-reader-setup)]
             [params (video-reader-get-params v)]
             [e (vp8enc-new params)]
             [buffsize (bin* 1024 256)]
             [outbuff (make-bytes buffsize)]
             [framerate (bin/ (VideoParams.fpsNum params) (VideoParams.fpsDen params))])
        ; grab-frame: int -> or FrameBuffer None
        (define (grab-frame ts)
          (cond [(video-reader-is-ready? v) (video-reader-get-frame v ts)]
                [else (None)]))
        ; encode-frame: or FrameBuffer None -> or FrameBuffer None
        (define (encode-frame frame)
          (cond [(None? frame) frame]
                [else (vp8enc-encode/return-frame e frame outbuff)]))
        ; grab/encode: -> or FrameBuffer None
        (define (grab/encode)
          (encode-frame (grab-frame (current-inexact-milliseconds))))
        (let loop ([v (grab/encode)] [fudge default-fudge])
          (cond [(FrameBuffer? v)
                 ;(printf "HIT: fudge ~a~n" fudge)
                 ; Frame received successfully. Pass it on and then wait until
                 ; the next frame should be active (modified by a factor to account for processing time)
                 (ask/send* "POST" relayer-curl (FrameBuffer->Frame v) (metadata type/webm))
                 (sleep* (bin* fudge framerate))
                 ; decrease fudge factor for next frame a la AIMD.
                 (loop (grab/encode) (if (bin>= fudge fudge-step)
                                         (bin- fudge fudge-step)
                                         0))]
                [(None? v)
                 ;(printf "MISS: fudge ~a~n" fudge)
                 (loop (grab/encode)
                       ; increase fudge factor for next frame a la AIMD.
                       (min* default-fudge (bin* 2 (if (zero? fudge)
                                                   fudge-step
                                                   fudge))))]))))))

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
      (let loop ([v (thread-receive)])
        (cond [(Frame? v) 
               (speex-decoder-decode d (bytes-length (Frame.data v)) (Frame.data v))
               (loop (thread-receive))]
              [(Quit? v)
               (speex-decoder-delete d)])))))