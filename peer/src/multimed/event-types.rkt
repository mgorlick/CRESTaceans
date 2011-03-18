#lang typed/racket

(require/typed racket
               [opaque Event evt?])

(struct: (pred) Eventof ([e : Event]
                         [p : (Any -> Boolean : pred)]))

(require/typed racket
               [sync (Event Event * -> Any)]
               [thread-receive-evt (-> Event)])

(: sync/t (All (a) (Eventof a) -> a))
(define (sync/t a)
  (let ([result (sync (Eventof-e a))])
    (cond [((Eventof-p a) result) result]
          [else (raise " ... ")])))

(: sync2/t (All (a b) (Eventof a) (Eventof b) -> (U a b)))
(define (sync2/t a b)
  (let ([result (sync (Eventof-e a) (Eventof-e b))])
    (cond [(or ((Eventof-p a) result)
               ((Eventof-p b) result)) result]
          [else (raise " ... ")])))

(: sync3/t (All (a b c) (Eventof a) (Eventof b) (Eventof c) -> (U a b c)))
(define (sync3/t a b c)
  (let ([result (sync (Eventof-e a) (Eventof-e b) (Eventof-e c))])
    (cond [(or ((Eventof-p a) result)
               ((Eventof-p b) result)
               ((Eventof-p c) result)) result]
          [else (raise " ... ")])))

(define (thread-receive-evt/t)
  (Eventof (thread-receive-evt) evt?))

(provide Event
         Eventof
         Eventof-e
         Eventof-p
         evt?
         sync/t
         sync2/t
         sync3/t
         thread-receive-evt/t)