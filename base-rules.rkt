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
        [(list (? thread? source) 'permit-update? (? dict? state) 
               (? boolean? vert?) (? boolean? horz?))
         (thread-send source (list (current-thread) 'permit-update
                                   (allow-vert? (get-fuel state) vert?)
                                   (allow-horz? (get-fuel state) horz?)))
         (loop)]
        ))
     ]))

(define (get-fuel state)
  (dict-ref state "player"))

(define (allow-vert? fuel vert?)
  (cond
    [vert? (> fuel 5)]
    [else #f]))

(define (allow-horz? fuel horz?)
  (cond
    [horz? (> fuel 1)]
    [else #f]))