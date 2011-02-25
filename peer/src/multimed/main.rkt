#lang racket

(require "util.rkt"
         "udp-source.rkt"
         "vorbisdec.rkt"
         (planet bzlib/thread:1:0))

(provide (all-defined-out))

(define (start port)
  (define pid (current-thread))
  (launch-threads ([t1 (vorbis-decode pid)]
                   [t2 (udp-source port pid t1)])
                  (make-immutable-hash `(("udp-source" . ,t2) ("vorbisdec" . ,t1)))))

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
            (printf "got Vorbis dec state, count = ~a~n" (dict-count states*))
            (if (not (= (dict-count thds) (dict-count states*)))
                (loop states*)
                states*))]))))
  
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