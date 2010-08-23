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
    
    ; wait for a message that specifies the sink thread for keypress events
    [() (receive/match
         [(list (? thread? source) 'start-request (? thread? sink))
          (thread-send source (list (current-thread) 'start-notification))
          (init sink)])]
    
    ; 1. read a key
    ; 2. send a keypress event to sink
    ; 3. goto 1
    [(sink) 
     (let loop ()
       (let ([key (read-char)])
         (cond [(or (eq? key #\d) (eq? key #\a) (eq? key #\w))
                (thread-send sink (list (current-thread) 'event-keyboardx key))]
               )
         (receive/match
          [(list (? thread? source) 'shutdown)
           (printf "keyboard reader shutting down~n")]
          [after 0 (loop)])))]))