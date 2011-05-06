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
  
  (define/contract (flush len bytes)
    (integer? bytes? . -> . void)
    (when (> len 0)
      (let-values ([(buffer λdisposal) (λrequest)])
        (bytes-copy! buffer 0 bytes 0 len)
        (thread-send receiver (make-FrameBuffer buffer len λdisposal)))))
  
  (λ ()
    (let loop ()
      (match (receive-killswitch/whatever is-signaller?)
        [(? die? _) (command/killswitch signaller receiver)
                    (reply/state-report signaller #f)]
        [(FrameBuffer data size λdisposal)
         (vp8enc-encode e size data flush)
         (λdisposal)
         (loop)]))))