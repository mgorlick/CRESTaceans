#lang racket

(require (planet bzlib/thread:1:0)
         "../libvorbis/libvorbis.rkt")

(define-syntax-rule (threadize arg1 arg2 ...)
  (list (current-thread) arg1 arg2 ...))

(define (udp-source port sinks)
  (let ([sock (udp-open-socket)]
        [buffer (make-bytes 10000)])
    (udp-bind! sock #f port)
    (let loop ()
      (let-values ([(len addr port) (udp-receive! sock buffer)])
        (for ([sink (in-list sinks)])
          (thread-send sink (threadize (bytes->immutable-bytes (subbytes buffer 0 len)) len)))
        (loop)))))

(define (go)
  (let* ([pid (current-thread)]
         [t (thread (λ () (udp-source 44000 (list pid))))])
    (let loop ()
      (receive/match
       [(list (? thread? thd)
              (? (and bytes? immutable?) buffer)
              (? number? len))
        (bytes->ogg-packet buffer len)
        (loop)]))))

(define (bytes->ogg-packet buffer len)
  (cond
    [(< len 28) (printf "weird packet: ~a bytes~n" len)]
    [else
     (let* ([data (subbytes buffer 0 (- len (+ (* 2 ogg64size) (* 3 longsize))))]
            [len+ (curry + (bytes-length data))]
            [bytes (subbytes buffer (len+ 0) (len+ longsize))]
            [bos (subbytes buffer (len+ longsize) (len+ (* 2 longsize)))]
            [eos (subbytes buffer (len+ (* 2 longsize)) (len+ (* 3 longsize)))]
            [granulepos (subbytes buffer (len+ (* 3 longsize)) (len+ (* 3 longsize) ogg64size))]
            [packetno (subbytes buffer (len+ (* 3 longsize) ogg64size) len)])
       (printf "data: ~a~n" data)
       (printf "packet len: ~a~n" (integer-bytes->integer bytes #t)))
     ]))

(go)