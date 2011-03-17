#lang racket

(require "util.rkt")
(provide make-udp-reader
         make-udp-writer)

(define/contract (make-udp-reader signaller inbound-host inbound-port receiver)
  (thread? (or/c #f string) exact-nonnegative-integer? thread? . -> . (-> void))
  (let ([socket (let ([s (udp-open-socket)]) (udp-bind! s inbound-host inbound-port) s)]
        [buffer (make-bytes 1000000)]
        [is-signaller? (make-thread-id-verifier signaller)])
    (λ ()
      (let loop ()
        (let ([signal (receive-killswitch/whatever is-signaller? #:block? #f)])
          (cond [(die? signal) (udp-close socket)
                               ;; ... retrieve packets in socket before closing ...
                               (command/killswitch signaller receiver)
                               (reply/state-report signaller #f)]
                [(no-message? signal) (let-values ([(len addr port) (udp-receive!* socket buffer)])
                                        (when len (thread-send receiver (subbytes buffer 0 len))))
                                      (loop)]))))))

(define/contract (make-udp-writer signaller remote-host remote-port)
  (thread? string? exact-nonnegative-integer? . -> . (-> void))
  (let ([socket (let ([s (udp-open-socket)]) (udp-connect! s remote-host remote-port) s)]
        [is-signaller? (make-thread-id-verifier signaller)])
    (λ ()
      (let loop ()
        (let ([buffer-or-die (receive-killswitch/whatever is-signaller? #:block? #t)])
          (cond [(die? buffer-or-die) (udp-close socket)
                                      (reply/state-report signaller #f)]
                [(bytes? buffer-or-die) (udp-send socket buffer-or-die)
                                        (loop)]))))))