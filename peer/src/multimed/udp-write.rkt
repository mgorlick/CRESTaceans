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
              [(? symbol? sig) 
               (when (die? sig)
                 (udp-close socket)
                 (reply/state-report signaller #f))])))))