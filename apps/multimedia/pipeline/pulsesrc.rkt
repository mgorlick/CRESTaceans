#lang racket/base

(require "../bindings/pulse/pulse.rkt"
         "util.rkt"
         "structs.rkt"
         "bufferpool.rkt"
         racket/contract)

(provide make-pulsesrc)

(define BUFFSIZE 4096)

(define/contract (make-pulsesrc signaller receiver)
  (thread? thread? . -> . (-> void))
  
  (define is-signaller? (make-thread-id-verifier signaller))
  (define-values (pool λrequest) (make-bufferpool-handler 20 BUFFSIZE))
  (define s (pulsesrc-new))
  
  (define (grab)
    (let-values ([(buffer λdisposal) (λrequest)])
      (if (pulsesrc-read s BUFFSIZE buffer)
          (thread-send receiver (make-FrameBuffer buffer BUFFSIZE λdisposal))
          (λdisposal))))
  
  (define (cleanup)
    (kill-thread pool)
    (command/killswitch signaller receiver)
    (reply/state-report signaller #f)
    (pulsesrc-delete s)
    (void))
  
  (λ ()
    (let loop ()
      (let ([msg (receive-killswitch/whatever is-signaller? #:block? #f)])
        (cond [(die? msg) (cleanup)]
              [(no-message? msg) (grab) (loop)])))))