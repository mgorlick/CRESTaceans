#lang racket

(require "../../../../bindings/vorbis/libvorbis.rkt"
         "../structs.rkt"
         "../util.rkt")

(provide make-ogg-demuxer)

(define READSIZE 8192)

(define (make-ogg-demuxer signaller filename receiver)
  (thread? path-string? thread? . -> . (-> void))
  
  (define d (ogg-demux-new))
  (define is-signaller? (make-thread-id-verifier signaller))
  (define seen-eof? #f)
  
  (define/contract (flush packet)
    (ogg-packet-pointer? . -> . void)
    (thread-send receiver (bytes-copy (ogg-packet-data packet))))
  
  (define (read/send loopback)
    (let ([bytes (read-bytes READSIZE)])
      (cond [(bytes? bytes) (ogg-demux-data d (bytes-length bytes) bytes flush)
                            (loopback)]
            [(eof-object? bytes) (printf "EOF~n")
                                 (set! seen-eof? #t)
                                 (loopback)])))
  
  (define (cleanup)
    (reply/state-report signaller #f)
    (command/killswitch signaller receiver))
  
  (λ ()
    (with-input-from-file filename
      (λ ()
        (let loop ()
          (let ([msg (receive-killswitch/whatever is-signaller? #:block? #f)])
            (cond [(die? msg) (cleanup)]
                  [(no-message? msg)
                   (if seen-eof?
                       (begin (sleep 1) (loop))
                       (read/send loop))])))))))