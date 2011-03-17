#lang racket

(require (planet bzlib/thread:1:0))
(provide (all-defined-out))

;; receive/killswitch and receive/state-report expect a procedure like what make-thread-id-verifier makes
(define/contract (make-thread-id-verifier signaller)
  (thread? . -> . (any/c . -> . boolean?))
  (λ (thd) 
    (and (thread? thd) (equal? signaller thd))))

;;; starting a pipeline
(define-syntax launch-threads
  (syntax-rules ()
    [(_ [id1 name1 thunk1] ...)
     (let* ([id1 (thread thunk1)]
            ...)
       (make-immutable-hash `((name1 . ,id1)
                              ...
                              )))]))

;;; signals for pipeline control
(define killswitch 'clone/die)

;; send a killswitch, or pass on a received killswitch to the next element of the pipeline
(define/contract (command/killswitch reply-to receiver)
  (thread? thread? . -> . void)
  (thread-send receiver (list reply-to killswitch)))

;; given a procedure that checks the validity of a killswitch source
;; look for one in the mbox and report whether it's valid or not
(define/contract (receive/killswitch id?)
  ((any/c . -> . boolean?) . -> . boolean?)
  #f)
  ;(receive/match [(list (? id? thd) (? (λ (sym) (equal? sym killswitch)) ks)) #t]
  ;               [after 0 #f]))

;;; gathering up state reports from a pipeline after a killswitch
(define state-report 'state-report)
;; pipeline components use reply/state-report to respond to a killswitch. all pipeline
;; components are required to reply with some state, though it may be empty
(define/contract (reply/state-report reply-to state)
  (thread? any/c . -> . void)
  (thread-send reply-to (list (current-thread) state-report state)))

;; receives one state report from the thread matching the thread handle identifier encoded in id?
(define/contract (receive/state-report id?)
  ((any/c . -> . boolean?) . -> . any/c)
  (receive/match [(list (? id? thd) (? (λ (sym) (equal? sym state-report)) sr) state) state]))

;; receive-state-report: functionally store the (string . whatever) in the dict,
;; where 'whatever' is what the thread identified by the thread handle sends in a state report
(define/contract (receive-state-report component-id thread-handle states)
  (string? thread? dict? . -> . dict?)
  (dict-set states component-id (receive/state-report (make-thread-id-verifier thread-handle))))

;; gather-states: a kind of map over a pipeline, yielding a dict of (string . any) from the
;; (string . thread) pipeline dict
(define/contract (gather-states pipeline)
  (dict? . -> . dict?)
  (let ([pl (dict->list pipeline)])
    (foldl receive-state-report (make-immutable-hash) (map car pl) (map cdr pl))))