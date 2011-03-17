#lang racket

(require "util.rkt"
         (planet bzlib/thread:1:0))
(provide make-udp-reader
         make-udp-writer)

(define/contract (make-udp-reader signaller inbound-host inbound-port receiver)
  (thread? (or/c #f string) exact-nonnegative-integer? thread? . -> . (-> void))
  (let ([socket (let ([s (udp-open-socket)]) (udp-bind! s inbound-host inbound-port) s)]
        [buffer (make-bytes 1000000)]
        [is-signaller? (make-thread-id-verifier signaller)])
    (λ ()
      (printf "listening on ~a:~a~n" inbound-host inbound-port)
      (let loop ()
        (cond [(receive/killswitch is-signaller?) (udp-close socket)
                                                  ;; ... retrieve packets in socket before closing ...
                                                  (reply/state-report signaller #f)
                                                  (command/killswitch signaller receiver)]
              [else (let-values ([(len addr port) (udp-receive!* socket buffer)])
                      (when len (thread-send receiver (subbytes buffer 0 len))))
                    (loop)])))))

(define/contract (make-udp-writer signaller remote-host remote-port)
  (thread? string? exact-nonnegative-integer? . -> . (-> void))
  (let ([socket (let ([s (udp-open-socket)]) (udp-connect! s remote-host remote-port) s)]
        [is-signaller? (make-thread-id-verifier signaller)])
    (λ ()
      (let loop ()
        (cond [(receive/killswitch is-signaller?) (udp-close socket)
                                                  (reply/state-report signaller #f)]
              [else (let ([buffer/? (receive/buffer)])
                      (when buffer/? (udp-send socket buffer/?))
                      (loop))])))))

(define/contract (receive/buffer)
  (-> bytes?)
  (receive/match [buff buff]))