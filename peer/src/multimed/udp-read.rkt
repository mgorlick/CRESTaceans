#lang typed/racket

(require "udp-types.rkt"
         "event-types.rkt"
         "util-types.rkt")

(require/typed "util.rkt"
               [receive-killswitch/whatever ((Any -> Boolean) -> Symbol)])

(provide make-udp-reader)

(: make-udp-reader (Thread (Option String) Exact-Nonnegative-Integer Thread -> (-> Void)))
(define (make-udp-reader signaller inbound-host inbound-port receiver)
  (let* ([socket (let ([s (udp-open-socket #f #f)]) (udp-bind! s inbound-host inbound-port) s)]
         [buffer (make-bytes 1000000)]
         [socket-evt (udp-receive!-evt/t socket buffer)]
         [mailbox-evt (thread-receive-evt/t)]
         [is-signaller? (make-thread-id-verifier signaller)])
    (λ ()
      (let: loop : Void ()
            (match (sync2/t mailbox-evt socket-evt)
              [(list len _ _) (thread-send receiver (subbytes buffer 0 len))
                              (loop)]
              [(? evt? _) (match (receive-killswitch/whatever is-signaller?)
                            [(? die? sig) (udp-close socket)
                                          ;; ... retrieve packets in socket before closing ...
                                          (command/killswitch signaller receiver)
                                          (reply/state-report signaller #f)]
                            [_ (loop)])])))))