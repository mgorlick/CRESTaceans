#lang racket/base

(require "util.rkt"
         "structs.rkt"
         racket/function
         racket/match)

(provide make-fork
         fork-add
         fork-remove)

(struct add-rcvr (thread))
(struct rmv-rcvr (thread))

(define (fork-add fork thd)
  (thread-send fork (add-rcvr thd))
  (void))

(define (fork-remove fork thd)
  (thread-send fork (rmv-rcvr thread))
  (void))

(define (make-fork signaller thds)
  (define is-signaller? (make-thread-id-verifier signaller))
  (λ ()
    (let loop ([thds thds])
      (let ([m (receive-killswitch/whatever is-signaller?)])
        (cond [(add-rcvr? m) (if (ormap (curry equal? (add-rcvr-thread m)) thds)
                                 (loop thds)
                                 (loop (cons (add-rcvr-thread m) thds)))]
              [(rmv-rcvr? m) (loop (filter (compose not (curry equal? (rmv-rcvr-thread m))) thds))]
              [(die? m) (for-each (curry command/killswitch signaller) thds)
                        (reply/state-report signaller #f)]
              
              [(bytes? m) (for-each (curryr thread-send m) thds)
                          (loop thds)]
              
              ;; pooling buffers and copying an incoming buffer to N outgoing buffers doesn't scale
              ;; in this case it's better to just let the GC handle it
              [(FrameBuffer? m) (match-let ([(FrameBuffer inbytes size λdisposal) m])
                                  (define frame (make-FrameBuffer (subbytes inbytes 0 size) size void))
                                  (map (curryr thread-send frame) thds)
                                  (λdisposal)
                                  (loop thds))])))))