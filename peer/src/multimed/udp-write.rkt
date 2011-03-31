#lang typed/racket

(require "util-types.rkt"
         "udp-types.rkt")
(provide make-udp-writer)

(require/typed "util.rkt"
               [receive-killswitch/whatever ((Any -> Boolean) -> (U Bytes Symbol))])

(: make-udp-writer (Thread String Exact-Nonnegative-Integer -> (-> Void)))
(define (make-udp-writer signaller remote-host remote-port)
  (let ([socket (let ([s (udp-open-socket #f #f)]) (udp-connect! s remote-host remote-port) s)]
        [is-signaller? (make-thread-id-verifier signaller)])
    (λ ()
      (let: loop : Void ()
            (match (receive-killswitch/whatever is-signaller?)
              [(? bytes? buffer) (udp-send socket buffer)
                                 (loop)]
              [(? (λ (s) (and (symbol? s) (die? s))) sig) (udp-close socket)
                                                          ;; ... retrieve packets in mailbox before dying ...
                                                          (reply/state-report signaller #f)])))))