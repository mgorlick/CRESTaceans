#lang racket

(provide (all-defined-out))

(define-syntax λ-loop
  (syntax-rules ()
    [(λ-loop ([b1 v1] ...)
             expr ...) (let ([b1 v1] ...)
                         (λ-loop expr ...))]
    [(λ-loop expr ...) (λ ()
                         (let loop ()
                           expr ...
                           (loop)))]))

(define-syntax dotimes
  (syntax-rules ()
    [(_ n expr ...)
     (for ([i (in-range n)]) expr ...)]))

(define-syntax signal/count
  (syntax-rules (:)
    [(_ sema (c : condexpr))
     (when condexpr (semaphore-post sema) (set! c (add1 c)))]))

(define-syntax signal-wait/count
  (syntax-rules ()
    [(_ sema ct)
     (dotimes ct (semaphore-wait sema))]))

(define (threadize . args)
  (cons (current-thread) args))

(define-syntax to-all
  (syntax-rules (<-)
    [(_ sinks <- arg1 ...)
     (match sinks
       [(? (listof thread?) ls) 
        (for ([s ls])
          (thread-send s (threadize arg1 ...)))]
       [(? thread? s) 
        (thread-send s (threadize arg1 ...))]
       )]))

(define-syntax launch-threads
  (syntax-rules ()
    [(_ [id1 name1 f1] ...)
     (let* ([id1 (thread (λ () f1))]
            ...)
       (make-immutable-hash `((name1 . ,id1)
                              ...
                              )))]))

(define (pipeline-kill lthread)
  (stream-for-each kill-thread lthread))