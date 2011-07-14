#lang racket/base

(provide (all-defined-out))

(define video-decoder
  '(let ([src/decoder
          (lambda ()
            (let ([d (vp8dec-new)])
              (printf "starting~n")
              (let loop ([v (thread-receive)])
                ;(printf "vp8 packet is ~a ms old~n" (FrameBuffer-age v))
                (cond [(FrameBuffer? v)
                       (vp8dec-decode d (FrameBuffer-size v) (FrameBuffer-data v))
                       (loop (thread-receive))]
                      [(Quit? v)
                       (vp8dec-delete d)
                       (printf "exiting~n")]))))])
     (src/decoder)))

(define (speex-decoder framesize)
  `(let ([src/decoder
          (lambda ()
            (let ([d (new-speex-decoder ,framesize)])
              (let loop ([v (thread-receive)])
                ;(printf "speex packet is ~a ms old~n" (FrameBuffer-age v))
                (cond [(FrameBuffer? v) 
                       (speex-decoder-decode d (FrameBuffer-size v) (FrameBuffer-data v))
                       (loop (thread-receive))]
                      [(Quit? v)
                       (speex-decoder-delete d)]))))])
     (src/decoder)))

(define vorbis-decoder
  '(let ([src/decoder
          (lambda ()
            (let* ([dec (vorbisdec-new)]
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
                    #f))))])
     (src/decoder)))