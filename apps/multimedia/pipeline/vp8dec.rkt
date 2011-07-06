#lang racket/base

(require "../bindings/vp8/vp8.rkt"
         "util.rkt"
         "structs.rkt"
         racket/contract
         racket/match)

(provide make-vp8-decoder)

(define/contract (make-vp8-decoder signaller)
  (thread? . -> . (-> void))
  
  (define is-signaller? (make-thread-id-verifier signaller))
  (define d (vp8dec-new))
  
  (λ ()
    (let loop ()
      (match (receive-killswitch/whatever is-signaller?)
        [(? die? _) (vp8dec-delete d)
                    (reply/state-report signaller #f)]
        [(? bytes? pkt) (vp8dec-decode d (bytes-length pkt) pkt)
                        (loop)]
        [(FrameBuffer data size λdisposal)
         (vp8dec-decode d size data)
         (λdisposal)
         (loop)]))))