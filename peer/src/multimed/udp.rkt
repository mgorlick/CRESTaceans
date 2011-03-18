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
        (match (receive-killswitch/whatever is-signaller? #:block? #f)
          [(? no-message? sig) (let-values ([(len addr port) (udp-receive!* socket buffer)])
                                 (when len (thread-send receiver (subbytes buffer 0 len))))
                               (loop)]
          [(? die? sig) (udp-close socket)
                        ;; ... retrieve packets in socket before closing ...
                        (command/killswitch signaller receiver)
                        (reply/state-report signaller #f)])))))

(define/contract (make-udp-writer signaller remote-host remote-port)
  (thread? string? exact-nonnegative-integer? . -> . (-> void))
  (let ([socket (let ([s (udp-open-socket)]) (udp-connect! s remote-host remote-port) s)]
        [is-signaller? (make-thread-id-verifier signaller)])
    (λ ()
      (let loop ()
        (match (receive-killswitch/whatever is-signaller?)
          [(? bytes? buffer) (udp-send socket buffer)
                             (loop)]
          [(? die? sig) (udp-close socket)
                        ;; ... retrieve packets in mailbox before dying ...
                        (reply/state-report signaller #f)])))))