#lang racket

(require
 (only-in
  "trie.rkt"
  define-accessor)
 (only-in
  "hash.rkt"
  hash/cons
  hash/eq/null
  hash/merge
  hash/ref
  hash/remove
  hash/persist?
  list/hash)
 
 )

(provide
 environ/null
 environ?
 environ/cons/1
 environ/cons/2
 environ/merge
 environ/remove
 environ/value
 list/environ
 )
          


;; A binding environment is a two-element vector v:
;; v[0] - the literal <environ/persist>
;; v[1] - an eq? based persitent hash table whose keys are the names of lexical variables (given as symbols) and
;;        arbirary Mischief values
(define-accessor environ/map 1)

(define ENVIRON/FLAVOR '<environ/persist>) ; Mischief type identifier for binding environments.

(define (environ/construct h)
  (vector ENVIRON/FLAVOR h))

(define environ/null (environ/construct hash/eq/null))

(define (environ? e)
  (and
   (vector? e)
   (= (vector-length e) 2)
   (eq? (vector-ref e 0) ENVIRON/FLAVOR)
   (hash/persist? (vector-ref e 1))))

;; Join binding k_1/v_1 to environ e returning the successor environ containing binding k_1/v_1.
;; e remains unchanged.
(define (environ/cons/1 e k_1 v_1)
  (environ/construct (hash/cons (environ/map e) k_1 v_1)))

;; Join bindings k_1/v_1 and k_2/v_2 to environ e returning the successor environ containing bindings k_1/v_1 and k_2/v_2.
;; e remains unchanged.
(define (environ/cons/2 e k_1 v_1 k_2 v_2)
  (environ/construct (list/hash (environ/map e) (list k_1 v_1 k_2 v_2))))

;; Given a list (input) of bindings (k_1 v_1 ... k_m v_m) join bindings k_1/v_1, ..., k_m/v_m to environ e returning the successor
;; environ containing k_1/v_1, ..., k_m/v_m.
;; e remains unchanged.
(define (list/environ e input)
  (environ/construct (list/hash (environ/map e) input)))

;; Let variables be a list (possibly empty) of variable names (k_1 ... k_m).
;; Remove the bindings k_1/v_1, ..., k_m/v_m from environ e returning the successor environ that does not contain any
;; of k_1/v_1, ..., k_m/v_m.
;; e remains unchanged.
(define (environ/remove e variables)
  ;; Given a list of keys (k_1 ... k_m) remove the pairs k_1/v_1, ..., k_m/v_m from persistent hash table h returning
  ;; the successor of h.
  (define (hash/list/remove h keys)
    (if (null? keys)
        h
        (hash/list/remove (hash/remove h (car keys)) (cdr keys))))
  
  (if (null? variables) e (environ/construct (hash/list/remove (environ/map e) variables))))


;; Returns the merge of environ beta into environ alpha returning a successor environ
;; where the bindings of beta are joined with those of alpha. The bindings of beta take precedence over those of alpha.
(define (environ/merge alpha beta)
  (environ/construct (hash/merge (environ/map alpha) (environ/map beta))))

(define (environ/value e symbol failure)
  (hash/ref (environ/map e) symbol failure))
  