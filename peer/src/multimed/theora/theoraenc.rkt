#lang racket

(require "../util.rkt"
         "../structs.rkt"
         "../../../../bindings/theora/theora.rkt")

(provide make-theora-encoder)

(define (make-theora-encoder signaller receiver)
  (λ ()
    (define is-signaller? (make-thread-id-verifier signaller))
    (define e (theoraenc-new))
    
    (define (flush packet)
      (let* ([data (ogg-packet-data packet)]
             [len (bytes-length data)])
        (when (> len 0) (thread-send receiver (bytes-copy data)))))
    
    (theoraenc-foreach-header e flush)
    (let loop ()
      (match (receive-killswitch/whatever is-signaller?)
        [(? die? _) (theoraenc-delete e) 
                    (command/killswitch signaller receiver)
                    (reply/state-report signaller #f)]
        [(FrameBuffer data framenum λdisposal)
         (theoraenc-data-in e data (bytes-length data) flush)
         (λdisposal)
         (loop)]))))