#lang racket

(require "bufferpool.rkt"
         "util.rkt"
         "structs.rkt")

(provide make-fork)

(define/contract (make-fork signaller thds)
  (thread? (listof thread?) . -> . (-> void))
  (define is-signaller? (make-thread-id-verifier signaller))
  ;; don't see a good way to estimate this buffer pool size in general,
  ;; 20/sink should work well enough despite eating a lot of ram
  (define-values (handler λrequest) (make-bufferpool-handler (* 5 (length thds)) (* 1024 1024)))
  (λ ()
    (let loop ()
      (match (receive-killswitch/whatever is-signaller?)
        [(? die? sig) (for-each (curry command/killswitch signaller) thds)
                      (reply/state-report signaller #f)]
        [(? bytes? b) (for-each (λ (t) (thread-send t bytes)) thds)
                      (loop)]
        [(FrameBuffer bytes size λdisposal)
         (for-each (λ (t)
                     (let-values ([(outbuf λdisp) (λrequest)])
                       (bytes-copy! outbuf 0 bytes 0 size)
                       (thread-send t (make-FrameBuffer outbuf size λdisp))))
                   thds)
         (λdisposal)
         (loop)]))))