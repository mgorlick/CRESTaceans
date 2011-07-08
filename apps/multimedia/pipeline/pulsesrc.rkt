#lang racket/base

(require "../bindings/pulse/pulse.rkt"
         "util.rkt"
         "structs.rkt"
         "bufferpool.rkt"
         racket/contract)

(provide (all-defined-out))

(struct pulse-settings (channels rate buffer-size))

(define/contract (make-pulsesrc signaller settings receiver)
  (thread? pulse-settings? thread? . -> . (-> void))
  
  (define BUFFSIZE (pulse-settings-buffer-size settings))
  
  (define is-signaller? (make-thread-id-verifier signaller))
  (define-values (pool λrequest) (make-bufferpool-handler 200 BUFFSIZE))
  (define s (pulsesrc-new (pulse-settings-rate settings) (pulse-settings-channels settings)))
  
  (define (grab)
    (let-values ([(buffer λdisposal) (λrequest)])
      (define ts (current-inexact-milliseconds))
      (if (pulsesrc-read s BUFFSIZE buffer)
          (thread-send receiver (make-FrameBuffer buffer BUFFSIZE λdisposal ts))
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