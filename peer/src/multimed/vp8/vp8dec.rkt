#lang racket

(require "../../../../bindings/vp8/vp8.rkt"
         "../util.rkt"
         "../structs.rkt")

(provide make-vp8-decoder)

(define/contract (make-vp8-decoder signaller)
  (thread? . -> . (-> void))
  
  (define is-signaller? (make-thread-id-verifier signaller))
  (define d (vp8dec-new))
  
  (λ ()
    (let loop ()
      (match (receive-killswitch/whatever is-signaller?)
        [(? die? _) (reply/state-report signaller #f)]
        [(FrameBuffer data size λdisposal)
         (vp8dec-decode d size data)
         (λdisposal)
         (loop)]))))