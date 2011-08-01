#lang racket/base

(provide (all-defined-out))

(define (relayer metadata)
  `(let relay ([curls (hash/new hash/equal/null)]
               [v (thread-receive)])
     
     (cond [(AddCURL? v)
            (relay (hash/cons curls (AddCURL.curl v) #f)
                   (thread-receive))]
           
           [(RemoveCURL? v)
            (ask/send* "POST" (RemoveCURL.curl v) (Quit) '(("is-a" . "quit-message")))
            (relay (hash/remove curls (RemoveCURL.curl v))
                   (thread-receive))]
           
           [(Frame? v)
            (let sendloop ([c (hash/keys curls)])
              (cond [(null? c) (void)]
                    [else (ask/send* "POST" (car c) v)
                          (sendloop (cdr c))]))
            (relay curls (thread-receive))]
           
           [else (printf "relay else: ~a~n" v)])))

;; -----
;; VIDEO
;; -----

(define video-decoder
  '(lambda (my-curl relayer-curl)
     (ask/send* "POST" relayer-curl (AddCURL my-curl) '(("is-a" . "curl")))
     (let ([d (vp8dec-new)])
       (printf "starting~n")
       (let loop ([v (thread-receive)])
         (cond [(Frame? v)
                (vp8dec-decode d (bytes-length (Frame.data v)) (Frame.data v))
                (loop (thread-receive))]
               [(Quit? v)
                (vp8dec-delete d)
                (printf "exiting~n")]
               [else (printf "not a message: ~a~n" v)])))))

(define video-reader/encoder
  '(lambda (relayer-curl)
     (let* ([default-fudge 0.5]
            [fudge-step 0.01]
            [v (video-reader-setup)]
            [params (video-reader-get-params v)]
            [e (vp8enc-new params)]
            [buffsize (* 1024 256)]
            [outbuff (make-bytes buffsize)]
            [framerate (/ (VideoParams.fpsNum params) (VideoParams.fpsDen params))])
       
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
       
       ; decrease-fudge: int -> int
       ; calculate the next fudge step by decreasing down to zero, additively. 
       ; called only when frame was successfully captured.
       (define (decrease-fudge current-fudge)
         (if (>= current-fudge fudge-step)
             (- current-fudge fudge-step)
             0))
       
       ; increase-fudge: int -> int
       ; calculate the next fudge step by increasing up to the default, multiplicatively.
       ; called only when the next frame could not be captured due to camera delay
       (define (increase-fudge current-fudge)
         (min default-fudge 
              (* 2 (if (zero? current-fudge)
                       fudge-step
                       current-fudge))))
       
       (define (fwd-frame fb)
         (ask/send* "POST" relayer-curl (FrameBuffer->Frame fb) '(("content-type" . "video/webm"))))
       
       ; loop: or FrameBuffer None, int
       (let loop ([fb (grab/encode)] [fudge default-fudge])
         (cond [(FrameBuffer? fb)
                ; Frame received successfully. Pass it on and then wait until
                ; the next frame should be active (modified by a factor to account for processing time)
                (fwd-frame fb)
                (sleep (* fudge framerate))
                (loop (grab/encode) (decrease-fudge fudge))]
               
               [(None? fb)
                (loop (grab/encode) (increase-fudge fudge))])))))

;; -----
;; AUDIO
;; -----

(define (audio-reader/speex-encoder echomod targeturl)
  `(let* ([enc (new-speex-encoder ,echomod)]
          [outbuff (make-bytes 1000)])
     (let loop ([ts (current-inexact-milliseconds)]
                [available (speex-encoder-encode (vector-ref enc 0) outbuff)])
       (ask/send* "POST" ,targeturl 
                  (Frame (subbytes outbuff 0 available) ts)
                  '(("content-type" . "audio/speex")))
       (loop (current-inexact-milliseconds)
             (speex-encoder-encode (vector-ref enc 0) outbuff)))))

(define (speex-decoder framesize)
  `(let ([d (new-speex-decoder ,framesize)])
     (let loop ([v (thread-receive)])
       (cond [(Frame? v) 
              (speex-decoder-decode d (bytes-length (Frame.data v)) (Frame.data v))
              (loop (thread-receive))]
             [(Quit? v)
              (speex-decoder-delete d)]))))