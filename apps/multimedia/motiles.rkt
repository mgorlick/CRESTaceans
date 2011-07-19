#lang racket/base

(provide (all-defined-out))

(define (relayer metadata)
  `(let relay ([curls (hash/new hash/equal/null)]
               [v (thread-receive)])
     
     (cond [(AddCURL? v)
            (relay (hash/cons curls (AddCURL.curl v) #f)
                   (thread-receive))]
           
           [(RemoveCURL? v)
            (ask/send* "POST" (RemoveCURL.curl v) (Quit))
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
  '(let ([d (vp8dec-new)])
     (printf "starting~n")
     (let loop ([v (thread-receive)])
       (cond [(Frame? v)
              (vp8dec-decode d (bytes-length (Frame.data v)) (Frame.data v))
              (loop (thread-receive))]
             [(Quit? v)
              (vp8dec-delete d)
              (printf "exiting~n")]
             [else (printf "not a message: ~a~n" v)]))))

(define (video-reader/encoder target)
  `(let* ([v (video-reader-setup)]
          [params (video-reader-get-params v)]
          [e (vp8enc-new params)]
          [buffsize (* 1024 256)]
          [outbuff (make-bytes buffsize)]
          [grab-frame
           ;; Number -> or (None) (FrameBuffer)
           (lambda (ts)
             (cond [(video-reader-is-ready? v) (video-reader-get-frame v ts)]
                   [else (None)]))]
          [encode-frame
           ;; or (None) (FrameBuffer) -> or (None) (FrameBuffer)
           (lambda (frame)
             (cond [(None? frame) frame]
                   [else (vp8enc-encode/return-frame e frame outbuff)]))]
          [grab/encode (lambda () (encode-frame (grab-frame (current-inexact-milliseconds))))])
     
     (sleep (/ (VideoParams-fpsNum params) (VideoParams-fpsDen params)))
     (let loop ([v (grab/encode)])
       (cond [(FrameBuffer? v)
              ; Frame received successfully. Pass it on and then wait until
              ; the next frame should be active (modified by a fudge factor to account for processing time)
              (thread-send ,target (FrameBuffer->Frame v))
              (sleep (* 0.5 (/ (VideoParams-fpsNum params) (VideoParams-fpsDen params))))]
             [(None? v)
              ; oops, our fudge factor was off - just sleep some small amount of time before checking again
              (sleep 0.025)])
       (loop (grab/encode)))))

;; -----
;; AUDIO
;; -----

(define (audio-reader/encoder echomod targeturl)
  `(let* ([enc (new-speex-encoder ,echomod)]
          [outbuff (make-bytes 1000)])
     (let loop ([ts (current-inexact-milliseconds)]
                [available (speex-encoder-encode (vector-ref enc 0) outbuff)])
       (ask/send* "POST" ,targeturl 
                  (vector (subbytes outbuff 0 available) ts)
                  '(("content-type" . "audio/speex")))
       (loop (current-inexact-milliseconds)
             (speex-encoder-encode (vector-ref enc 0) outbuff)))))

(define (speex-decoder framesize)
  `(let ([d (new-speex-decoder ,framesize)])
     (let loop ([v (thread-receive)])
       (cond [(FrameBuffer? v) 
              (speex-decoder-decode d (FrameBuffer-size v) (FrameBuffer-data v))
              (loop (thread-receive))]
             [(vector? v)
              (speex-decoder-decode d (bytes-length (vector-ref v 0)) (vector-ref v 0))
              (loop (thread-receive))]
             [(Quit? v)
              (speex-decoder-delete d)]))))