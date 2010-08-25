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
        [(list (? thread? source) 'permit-update?
               (? dict? state) 
               (? integer? xdir)
               (? integer? ydir))
         (printf "update?: ~s ~s ~n" xdir ydir)
         (thread-send source (list (current-thread) 'permit-update!
                                   (allow-horz? (get-fuel state) xdir)
                                   (allow-vert? (get-fuel state) ydir) 
                                   ))
         (loop)]
        
        [(list (? thread? source) 'shutdown)
         (for/list ([e extensions])
           (thread-send e (list (current-thread) 'shutdown)))
         (printf "rules shutting down~n")]
        ))
     ]))

(define (get-fuel state)
  (dict-ref state "player"))

(define (allow-vert? fuel ydir)
  (cond
    [(> fuel (* 5 ydir)) ydir]
    [else 0.0]))

(define (allow-horz? fuel xdir)
  (cond
    [(> fuel xdir) xdir]
    [else 0.0]))