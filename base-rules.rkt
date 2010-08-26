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
               (? integer? xdir)
               (? integer? ydir))
         (let ((allow-horz (allow-horz? (get-fuel state) xdir)))
           (if (not (allow-horz . = . 0.0)) (subt-fuel! state allow-horz 0.0) (void))
           (let ((allow-vert (allow-vert? (get-fuel state) ydir)))
             (if (not (allow-vert . = . 0.0)) (subt-fuel! state 0.0 allow-vert) (void))
             (thread-send source (list (current-thread) 'permit-update!
                                       allow-horz
                                       allow-vert
                                       state
                                       ))))
             (loop)]
))
]))

(define (get-fuel state)
  (dict-ref state "player"))

(define horz-price 1)
(define vert-price 5)

(define (allow-vert? fuel ydir)
  (cond
    [(>= fuel (* vert-price (abs ydir))) ydir]
    [else 0.0]))

(define (allow-horz? fuel xdir)
  (cond
    [(>= fuel (* horz-price (abs xdir))) xdir]
    [else 0.0]))

(define (subt-fuel! state xdir ydir)
  (dict-set! state "player" (- (get-fuel state) (* vert-price (abs ydir)) (* horz-price (abs xdir))))
  state)