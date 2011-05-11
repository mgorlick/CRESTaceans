#lang racket

(require "../util.rkt"
         "../structs.rkt"
         "../bufferpool.rkt"
         "../../../../bindings/theora/theora.rkt")

(provide make-theora-encoder)

(define (make-theora-encoder signaller receiver)
  (λ ()
    (define is-signaller? (make-thread-id-verifier signaller))
    (define e (theoraenc-new))
    (define-values (handler λrequest) (make-bufferpool-handler 20 (* 1024 1024)))
    
    (define (flush packet)
      (let* ([len (ogg-packet-size packet)])
        (when (> len 0)
          (let-values ([(buffer λdisposal) (λrequest)])
            (bytes-copy! buffer 0 (ogg-packet-data packet) 0 len)
            (thread-send receiver (make-FrameBuffer buffer len λdisposal))))))
    
    (theoraenc-foreach-header e flush)
    (let loop ()
      (match (receive-killswitch/whatever is-signaller?)
        [(? die? _) (theoraenc-delete e) 
                    (command/killswitch signaller receiver)
                    (reply/state-report signaller #f)]
        [(FrameBuffer data size λdisposal)
         (theoraenc-data-in e data size flush)
         (λdisposal)
         (loop)]))))