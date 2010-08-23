#lang racket

(require (planet bzlib/thread))
(provide/contract [start-peer (-> void?)])

(define (start-peer)
  (receive/match
   [(list (? thread? source) 'spawn-request)
    (thread-send source (list (current-thread) 'spawn-notification (thread init)))
    (start-peer)]))

; these are parameters for testing purposes. test script replaces their
; default values to shorten the time between sending messages, and
; to log messages instead of actually sending them
(define send-out 
  (make-parameter (lambda (sink xdir ydir) 
                    (thread-send sink (list (current-thread) 'event-control xdir ydir)))))
(define delta
  (make-parameter (1000 . / . 60)))

(define init
  (case-lambda
    [() (receive/match
         [(list (? thread? source) 'start-request (? thread? sink))
          (thread-send source (list (current-thread) 'start-notification))
          (init sink)])]
    
    ;; (1) read a keyboard event notification from the thread mailbox, if exists
    ;; (2) then, (a) either send off the current compilation of keyboard events
    ;;               in terms of their effect on control and reset the timer,
    ;;           (b) or keep reading events until the timer goes off
    [(sink) (call/cc
             (lambda (k) 
               (let loop ((xdir 0.0)
                          (ydir 0.0)
                          (a (alarm-evt (+ (delta) (current-inexact-milliseconds)))))
                 (receive/match [(list (? thread? source) 'event-keyboard #\d)
                                 (set! ydir (add1 ydir))]
                                [(list (? thread? source) 'event-keyboard #\a)
                                 (set! ydir (sub1 ydir))]
                                [(list (? thread? source) 'event-keyboard #\w)
                                 (set! xdir (sub1 xdir))]
                                [(list (? thread? source) 'shutdown)
                                 (printf "control shutting down~n")
                                 (k #f)]
                                [after 0])
                 (let ((makealarm? (sync/timeout 0 a)))
                   (cond [makealarm?
                          ((send-out) sink xdir ydir)
                          (loop 0.0 0.0 (alarm-evt (+ (delta) (current-inexact-milliseconds))))]
                         [else (loop xdir ydir a)])))))]))

; should always print (-1, 0)
(define (test)
  (parameterize ([send-out (lambda (sink xdir ydir)
                             (printf "(~s, ~s) ~n" xdir ydir))]
                 [delta 0])
    (let ((t (thread (lambda () (init #f)))))
      (let loop ()
        (sleep 0)
        (thread-send t (list (current-thread) 'event-keyboard #\w))
        (loop)))))