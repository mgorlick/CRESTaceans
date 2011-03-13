#lang racket

(require "util.rkt"
         "vorbisdec.rkt"
         (planet bzlib/thread:1:0))

(provide (all-defined-out))

;; start: launch a UDP listener on a given socket, which forwards each packet
;; to the thread mailbox of a Vorbis decoder running in a separate thread
(define (start port [initial-vorbis-state #f])
  (define pid (current-thread))
  (launch-threads [t1 "vorbisdec" (if initial-vorbis-state
                                      (vorbis-decode pid port initial-vorbis-state)
                                      (vorbis-decode pid port))]))

;; pause/move/restart:
;; first, send the clone-state-and-die message to the head of the pipeline, which causes
;; the component there to (1) forward the message to its sinks, and
;; (2) report its current state to this thread. the state-reporting constraint
;; holds true for all threads which listen for clone-state-and-die messages.

;; second, wait for all the states to come in. this function expects every
;; element in the pipeline to report a state.

;; third, start a new instance of each component in the old pipeline, yielding a new
;; pipeline. unlike before, we explicitly provide initialization component states
;; when the component requires it (e.g., the codec state for the vorbis decoder).
(define (pause/move/restart pipeline new-port)
  
  (define (receive-state-report component-key states)
    (receive/match
     [(list (? (curry equal? (dict-ref states component-key)) thread) 'state-report newstate)
      (hash-set states component-key newstate)]))
  
  (define (gather-states)
    (foldl receive-state-report pipeline (dict-keys pipeline)))
  
  (to-all (dict-ref pipeline "vorbisdec") <- 'clone-state-and-die)
  (let ([states (gather-states)])
    (start new-port (dict-ref states "vorbisdec"))))

(define pipeline (start 5000))

(define (pmr) (set! pipeline (pause/move/restart pipeline 5001)))