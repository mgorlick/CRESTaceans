#lang racket

(provide (all-defined-out))

(define (threadize . args)
  (cons (current-thread) args))

(define-syntax to-all
  (syntax-rules (<-)
    [(_ sinks <- arg1 ...)
     (match sinks
       [(? thread? s) (thread-send s (threadize arg1 ...))]
       [(? (listof thread?) ls) 
        (for ([s ls])
          (thread-send s (threadize arg1 ...)))])]))

(define-syntax threads
  (syntax-rules (->)
    [(_ ([id1 -> f1] ... [idn -> fn]) body ...)
     (let* ([id1 (thread (λ () f1))]
            ...
            [idn (thread (λ () fn))])
       body ...)]))

(define (pipeline-kill lthread)
  (stream-for-each kill-thread lthread))