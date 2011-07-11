#lang racket/base

(require racket/contract
         racket/dict
         racket/function
         racket/match)
(provide (all-defined-out))

(define-syntax-rule (define-thread id body ...)
  (define id (thread (λ () body ...))))

;; receive/killswitch and receive/state-report expect a procedure made by
;; make-thread-id-verifier
(define/contract (make-thread-id-verifier signaller)
  (thread? . -> . (any/c . -> . boolean?))
  (λ (thd) 
    (and (thread? thd) (equal? signaller thd))))

;;; -------------------
;;; starting a pipeline
;; for each { name : identifier thunk } let*-like binding, 
;; starts a thread with the thunk, binds the thread to the identifier
;; and then produces a dict with the (name . thread) pairs.
;; any exprs following the let*-like bindings are evaluated 
;; for their side effects before the dict is produced.
;; in addition, the dict is guaranteed to contain the pipeline elements
;; in the order they are specified in the let*-like bindings 
;; (which is usually right-to-left, since later pipeline
;; elements have to be specified earlier).
(define-syntax make-pipeline
  (syntax-rules (:)
    [(_ ([name1 : id1 thunk1] ...) expr ...)
     (let* ([id1 (thread thunk1)]
            ...)
       expr ...
       `((name1 . ,id1)
         ...
         ))]))

(define/contract (pipeline-add p name thunk)
  (dict? string? (-> void) . -> . dict?)
  (cons `(,name . ,(thread thunk)) p))

;;; ------------------------------------------------
;;; signals for pipeline control from the flowmaster
(define killswitch 'clone/die)
(define die? (curry equal? killswitch))
(define no-message 'no-message)
(define no-message? (curry equal? no-message))

(define/contract (command reply-to receiver command)
  (thread? thread? symbol? . -> . void)
  (thread-send receiver (list reply-to command)))

;; send a killswitch, or pass on a received killswitch to the next element of the pipeline
(define/contract (command/killswitch reply-to receiver)
  (thread? thread? . -> . void)
  (command reply-to receiver killswitch))

;; receive a killswitch, or whatever else. at minimum, 
;; the pipeline element must test the output for die?
;; and if #:block? is #f, the pipeline element must test for no-message?
(define/contract (receive-killswitch/whatever id? #:block? [block? #t])
  ([(any/c . -> . boolean?)] [#:block? boolean?] . ->* . any)
  (if (sync/timeout (if block? +inf.0 0) (thread-receive-evt))
      (match (thread-receive)
        [(list (? id? thd) (? symbol? cntrl)) cntrl]
        [whatever whatever])
      no-message))

;;; -------------------------------------------------------------
;;; gathering up state reports from a pipeline after a killswitch
(define state-report 'state-report)
(define state-report? (curry equal? state-report))

;; pipeline components use reply/state-report to respond to a killswitch. all pipeline
;; components are required to reply with some state, though it may be empty
(define/contract (reply/state-report reply-to state)
  (thread? any/c . -> . void)
  (thread-send reply-to (list (current-thread) state-report state)))

;; receives one state report from the thread matching the thread handle identifier encoded in id?
(define/contract (receive/state-report id?)
  ((any/c . -> . boolean?) . -> . any/c)
  (match (thread-receive)
    [(list (? id? thd) (? state-report? sr) state)
     state]))

;; receive-state-report: functionally store the (string . whatever) in the dict,
;; where 'whatever' is what the thread identified by the thread handle sends in a state report
(define/contract (receive-state-report component-id thread-handle states)
  (string? thread? dict? . -> . dict?)
  (printf "getting state from ~a~n" component-id)
  (dict-set states component-id (receive/state-report (make-thread-id-verifier thread-handle))))

;; gather-states: a kind of map over a pipeline, yielding a dict of (string . any) from the
;; (string . thread) pipeline dict
(define/contract (gather-states pipeline)
  (dict? . -> . dict?)
  (let ([pl (dict->list pipeline)])
    (foldr receive-state-report '() (map car pl) (map cdr pl))))