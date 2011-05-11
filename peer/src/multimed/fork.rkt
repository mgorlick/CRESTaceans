#lang typed/racket

(require "util-types.rkt"
         "structs.rkt")

(require/typed "util.rkt"
               [receive-killswitch/whatever ((Any -> Boolean) -> (U Symbol AddRcvr RmvRcvr Bytes FrameBuffer))])

(provide make-fork
         fork/add
         fork/remove)

(define-type AddRcvr (Pair 'AddRcvr Thread))
(define-type RmvRcvr (Pair 'RmvRcvr Thread))
(define-predicate add-rcvr? AddRcvr)
(define-predicate rmv-rcvr? RmvRcvr)

(: fork/add (Thread Thread -> Void))
(define (fork/add fork thd)
  (thread-send fork (cons 'AddRcvr thd))
  (void))

(: fork/remove (Thread Thread -> Void))
(define (fork/remove fork thd)
  (thread-send fork (cons 'RmvRcvr thd))
  (void))

(: make-fork (Thread (Listof Thread) -> (-> Void)))
(define (make-fork signaller thds)
  (define is-signaller? (make-thread-id-verifier signaller))
  (λ ()
    (let loop ([thds thds])
      (let ([m (receive-killswitch/whatever is-signaller?)])
        (cond [(add-rcvr? m) (loop (cons (cdr m) thds))]
              [(rmv-rcvr? m) (loop (filter (λ: ([t : Thread]) (not (equal? t (cdr m)))) thds))]
              [(die? m) (for-each (curry command/killswitch signaller) thds)
                        (reply/state-report signaller #f)]
              
              [(bytes? m) (for-each (λ: ([t : Thread]) (thread-send t m)) thds)
                          (loop thds)]
              ;; pooling buffers and copying an incoming buffer to N outgoing buffers doesn't scale
              ;; in this case it's better to just let the GC handle it
              [(FrameBuffer? m) (match-let ([(FrameBuffer bytes size λdisposal) m])
                                  (define outbuf (subbytes bytes 0 size))
                                  (map (λ: ([t : Thread])
                                           (thread-send t (FrameBuffer outbuf size (λ () (void)))))
                                       thds)
                                  (λdisposal)
                                  (loop thds))])))))