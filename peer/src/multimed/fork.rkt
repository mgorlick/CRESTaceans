#lang racket

(require "util.rkt"
         "structs.rkt")

(provide make-fork)

(define/contract (make-fork signaller thds)
  (thread? (listof thread?) . -> . (-> void))
  (define is-signaller? (make-thread-id-verifier signaller))
  (λ ()
    (let loop ([thds thds])
      (match (receive-killswitch/whatever is-signaller?)
        [(? die? _) (for-each (curry command/killswitch signaller) thds)
                    (reply/state-report signaller #f)]
        
        [(? bytes? b) (for-each (λ (t) (thread-send t bytes)) thds)
                      (loop thds)]
        ;; pooling buffers and copying an incoming buffer to N outgoing buffers doesn't scale
        ;; in this case it's better to just let the GC handle it
        [(FrameBuffer bytes size λdisposal)
         (define outbuf (subbytes bytes 0 size))
         (for-each (λ (t)
                     (thread-send t (make-FrameBuffer outbuf size (λ () (void)))))
                   thds)
         (λdisposal)
         (loop thds)]))))