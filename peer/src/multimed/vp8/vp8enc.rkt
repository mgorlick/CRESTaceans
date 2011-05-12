#lang racket

(require "../../../../bindings/vp8/vp8.rkt"
         "../util.rkt"
         "../structs.rkt"
         "../bufferpool.rkt")

(provide make-vp8-encoder)

(define BUFSIZE (* 1024 1024))

(define/contract (make-vp8-encoder signaller receiver)
  (thread? thread? . -> . (-> void))
  
  (define is-signaller? (make-thread-id-verifier signaller))
  (define e (vp8enc-new))
  (define-values (handler λrequest) (make-bufferpool-handler 20 BUFSIZE))
  
  (λ ()
    (let loop ()
      (match (receive-killswitch/whatever is-signaller?)
        [(? die? _) (vp8enc-delete e)
                    (command/killswitch signaller receiver)
                    (reply/state-report signaller #f)]
        [(FrameBuffer data size λdisposal)
         (let-values ([(outbuff λreturn) (λrequest)])
           (let ([written (vp8enc-encode e size data BUFSIZE outbuff)])
             (thread-send receiver (make-FrameBuffer outbuff written λreturn))))
         (λdisposal)
         (loop)]))))