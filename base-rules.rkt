#lang racket

(require (planet bzlib/thread))
(provide/contract [start-peer (-> void?)])

(define (start-peer)
  (receive/match
   [(list (? thread? source) 'spawn-request)
    (thread-send source (list (current-thread) 'spawn-notification (thread init)))
    (start-peer)]))

(define init
  (case-lambda
    [() 
     (receive/match
      [(list (? thread? source) 'start-request)
       (thread-send source (list (current-thread) 'start-notification))
       (init '())])]
    
    [(extensions) 
     (let loop ()
       (receive/match
        [(list (? thread? source) 'shutdown)
         (for/list ([e extensions])
           (thread-send e (list (current-thread) 'shutdown)))
         (printf "rules shutting down~n")]
        
        [(list (? thread? source) 'permit-update?
               (? dict? state) 
               (? integer? mvmt-coef))
           (let ((allow-mvmt (allow-mvmt? (get-fuel state) mvmt-coef)))
             (if (not (= allow-mvmt 0.0)) (subt-fuel! state allow-mvmt) #f)
             (thread-send source (list (current-thread) 'permit-update!
                                       allow-mvmt state
                                       )))
             (loop)]
))
]))

(define (get-fuel state)
  (dict-ref state "player"))

(define vert-price 1)

(define (allow-mvmt? fuel ydir)
  (cond
    [(>= fuel (* vert-price (abs ydir))) ydir]
    [else 0.0]))

(define (subt-fuel! state mvmt-coef)
  (dict-set! state "player" (- (get-fuel state) (* vert-price (abs mvmt-coef))))
  state)