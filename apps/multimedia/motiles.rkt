#lang racket/base

(provide (all-defined-out))

;; -----
;; VIDEO
;; -----

(define video-decoder
  '(let ([d (vp8dec-new)])
     (printf "starting~n")
     (let loop ([v (thread-receive)])
       ;(printf "vp8 packet is ~a ms old~n" (FrameBuffer-age v))
       (cond [(FrameBuffer? v)
              (vp8dec-decode d (FrameBuffer-size v) (FrameBuffer-data v))
              (loop (thread-receive))]
             [(Quit? v)
              (vp8dec-delete d)
              (printf "exiting~n")]))))

(define (video-reader/encoder target)
  `(let* ([v (video-reader-setup)]
          [params (video-reader-get-params v)]
          [e (vp8enc-new (VideoParams-width params) (VideoParams-height params)
                         (VideoParams-fpsNum params) (VideoParams-fpsDen params))]
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
     (let loop ([v (grab/encode)])
       (cond [(FrameBuffer? v)
              ; Frame received successfully. Pass it on and then wait until
              ; the next frame should be active (modified by a fudge factor to account for processing time)
              (thread-send ,target v)
              (sleep (* 0.5 (/ (VideoParams-fpsNum params) (VideoParams-fpsDen params))))]
             [(None? v)
              ; oops, our fudge factor was off - just sleep some small amount of time before checking again
              (sleep 0.025)])
       (loop (grab/encode)))))

;; -----
;; AUDIO
;; -----

(define (speex-decoder framesize)
  `(let ([d (new-speex-decoder ,framesize)])
     (let loop ([v (thread-receive)])
       ;(printf "speex packet is ~a ms old~n" (FrameBuffer-age v))
       (cond [(FrameBuffer? v) 
              (speex-decoder-decode d (FrameBuffer-size v) (FrameBuffer-data v))
              (loop (thread-receive))]
             [(Quit? v)
              (speex-decoder-delete d)]))))

(define vorbis-decoder
  '(let* ([dec (vorbisdec-new)]
          [packet-type (lambda (buffer len)
                         (cond [(zero? len) 'empty]
                               [(= 1 (bitwise-and 1 (bytes-ref buffer 0))) 'header]
                               [else 'data]))]
          [handle-buffer (lambda (buffer len)
                           (cond [(not (vorbisdec-is-init dec))
                                  (cond [(equal? (packet-type buffer len) 'header)
                                         ;(printf "packet is header~n")
                                         (header-packet-in dec buffer len)]
                                        [else
                                         (printf "error: non-header received when decoder uninitialized~n")
                                         #f])]
                                 [else
                                  (and (equal? (packet-type buffer len) 'data)
                                       (data-packet-blockin dec buffer len))]))])
     (let loop ([v (thread-receive)])
       ;(printf "vorbis packet is ~a ms old~n" (FrameBuffer-age v))
       (if (handle-buffer (FrameBuffer-data v) (FrameBuffer-size v))
           (loop (thread-receive))
           #f))))