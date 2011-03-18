#lang racket

(require "util.rkt")
(provide make-udp-reader
         make-udp-writer)

(define/contract (make-udp-reader signaller inbound-host inbound-port receiver [buffer-size 1000000])
  (thread? (or/c #f string) exact-nonnegative-integer? thread? . -> . (-> void))
  (let* ([socket (let ([s (udp-open-socket)]) (udp-bind! s inbound-host inbound-port) s)]
         [buffer (make-bytes buffer-size)]
         [socket-evt (udp-receive!-evt socket buffer)]
         [mailbox-evt (thread-receive-evt)]
         [is-signaller? (make-thread-id-verifier signaller)])
    (λ ()
      (let loop ()
        (match (sync mailbox-evt socket-evt)
          [(list len address port) (thread-send receiver (subbytes buffer 0 len))
                                   (loop)]
          [(? evt? _) (match (receive-killswitch/whatever is-signaller?)
                        [(? die? sig) (udp-close socket)
                                      ;; ... retrieve packets in socket before closing ...
                                      (command/killswitch signaller receiver)
                                      (reply/state-report signaller #f)]
                        [_ (loop)])])))))

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