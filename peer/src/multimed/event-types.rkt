#lang typed/racket

(require/typed racket
               [opaque Event evt?])

(struct: (pred) Eventof ([e : Event]
                         [p : (Any -> Boolean : pred)]))

(require/typed racket
               [sync (Event Event * -> Any)]
               [thread-receive-evt (-> Event)])

(define-syntax sync*
  (syntax-rules ()
    [(_ ea ...)
     (let ([result (sync (Eventof-e ea) ...)])
       (cond [((Eventof-p ea) result) result]
             ...
             [else (raise (format "sync exception: post-sync type didn't match a declarated `eventof' type: ~a~n" result))]))]))

(define (thread-receive-evt/t)
  (Eventof (thread-receive-evt) evt?))

(provide Event
         Eventof
         Eventof-e
         Eventof-p
         evt?
         sync*
         thread-receive-evt/t)