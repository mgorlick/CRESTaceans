#lang racket

(require "../../../../bindings/vp8/vp8.rkt"
         "../util.rkt"
         "../structs.rkt"
         "../bufferpool.rkt")

(provide make-vp8-encoder)

(define/contract (make-vp8-encoder signaller receiver)
  (thread? thread? . -> . (-> void))
  
  (define is-signaller? (make-thread-id-verifier signaller))
  (define e (vp8enc-new))
  (define-values (handler λrequest) (make-bufferpool-handler 20 (* 1024 1024)))
  
  (λ ()
    (let loop ()
      (match (receive-killswitch/whatever is-signaller?)
        [(? die? _) (command/killswitch signaller receiver)
                    (reply/state-report signaller #f)]
        [(FrameBuffer data size λdisposal)
         (let-values ([(outbuff λreturn) (λrequest)])
           (let ([written (vp8enc-encode e size data outbuff)])
             (thread-send receiver (make-FrameBuffer outbuff written λreturn))))
         (λdisposal)
         (loop)]))))