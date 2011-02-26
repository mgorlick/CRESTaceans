#lang racket

(require "util.rkt"
         "udp-source.rkt"
         "vorbisdec.rkt"
         (planet bzlib/thread:1:0))

(provide (all-defined-out))

;; start: launch a UDP listener on a given socket, which forwards each packet
;; to the thread mailbox of a Vorbis decoder running in a separate thread
(define (start port)
  (define pid (current-thread))
  (launch-threads ([t1 (vorbis-decode pid)]
                   [t2 (udp-source port pid t1)])
                  (make-immutable-hash `(("udp-source" . ,t2) ("vorbisdec" . ,t1)))))

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
(define (pause/move/restart thds new-port)
  
  (define (gather-states)
    (let ([states (make-immutable-hash '())])
      (let loop ([states states])
        (receive/match
         [(list (? (curry equal? (dict-ref thds "udp-source")) udpsrc) state)
          (let ([states* (hash-set states "udp-source" state)])
            (printf "got UDP state~n")
            (if (not (= (dict-count thds) (dict-count states*)))
                (loop states*)
                states*))]
         
         [(list (? (curry equal? (dict-ref thds "vorbisdec")) vorbisdec) state)
          (let ([states* (hash-set states "vorbisdec" state)])
            (printf "got Vorbis dec state~n")
            (if (not (= (dict-count thds) (dict-count states*)))
                (loop states*)
                states*))]
         ))))
  
  (to-all (dict-ref thds "udp-source") <- 'clone-state-and-die)
  (let ([states (gather-states)]
        [pid (current-thread)])
    (printf "gathered states~n")
    (dict-for-each states (λ (k v) (printf "state: ~a, ~a~n" k v)))
    (launch-threads ([t1 (vorbis-decode (dict-ref states "vorbisdec") pid)]
                     [t2 (udp-source new-port pid t1)])
                    (make-immutable-hash `(("udp-source" . ,t2) ("vorbisdec" . ,t1))))))

(define pipeline (start 5000))

(define (pmr) (pause/move/restart pipeline 5001))