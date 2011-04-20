#lang typed/racket

(require "util-types.rkt"
         "udp-types.rkt")
(provide make-udp-writer)

(require/typed "util.rkt"
               [receive-killswitch/whatever ((Any -> Boolean) -> (U Bytes Symbol))])

(: make-udp-writer (Thread String Exact-Nonnegative-Integer -> (-> Void)))
(define (make-udp-writer signaller remote-host remote-port)
  (let ([socket (udp-open-socket #f #f)]
        [is-signaller? (make-thread-id-verifier signaller)])
    (λ ()
      (let: loop : Void ()
            (match (receive-killswitch/whatever is-signaller?)
              [(? bytes? buffer) (udp-send-to socket remote-host remote-port buffer)
                                 (loop)]
              [(? symbol? sig) 
               (when (die? sig)
                 (udp-close socket)
                 (reply/state-report signaller #f))])))))