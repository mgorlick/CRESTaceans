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
               (? integer? mvmt-coef)
               (? integer? rotate-coef))
         (let ((rotate-amt (allow-rotate? (get-fuel state) rotate-coef)))
           (subt-fuel! state rotate-amt rotate-price)
           (let ((mvmt-amt (allow-mvmt? (get-fuel state) mvmt-coef)))
             (subt-fuel! state mvmt-amt mvmt-price)
             (thread-send source (list (current-thread) 'permit-update!
                                       mvmt-amt rotate-amt state
                                       ))))
           (loop)]
))
]))

(define (get-fuel state)
  (dict-ref state "player"))

(define mvmt-price 5)
(define rotate-price 1)

(define (allow-rotate? fuel coef)
  (if (>= fuel (* rotate-price (abs coef)))
      coef
      (* coef (/ fuel rotate-price))))

(define (allow-mvmt? fuel coef)
  (if (>= fuel (* mvmt-price (abs coef))) 
      coef
      (* coef (/ fuel mvmt-price))))

(define (subt-fuel! state amt price)
  (dict-set! state "player" (- (get-fuel state) (* price (abs amt))))
  state)