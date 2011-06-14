#lang racket

;; Copyright 2010 Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Contact: mgorlick@acm.org

;; Persistent hash table based on the "Ideal Hash Tries" of Phil Bagwell using the Java implementation of
;; Andrew McKinlay as a starting point.
;; (See http://thesoftwarelife.blogspot.com/2009/10/java-immutable-persistent-map.html
;;  for a brief overview of McKinlay's implementation. The source code is available at
;;  http://suneido.svn.sourceforge.net/viewvc/suneido/jsuneido/src/suneido/util/PersistentMap.java?view=markup ).

(require "trie.rkt")

(provide
 ; Type test.
 hashpersist?
 hash/eq?
 hash/eqv?
 hash/equal?
 
 ; Empty hash tables.
 hash/eq/null
 hash/eqv/null
 hash/equal/null

 ; Hash table constructors.
 hash/new
 list/hash
 pairs/hash
 hash/cons
 hash/remove
 hash/merge
 
 ; Hash table deconstructors.
 hash/list
 hash/pairs
 hash/keys
 
 ; Hash table lookup.
 hash/ref
 hash/car
 hash/cdr

 hash/length
 hash/empty?
 hash/contains?

 ; Hash table combinators.
 hash/fold
 hash/map
 hash/filter
 hash/partition
 
 ; Exported solely for the sake of serialize/deserialize.
 hash/equality
 hash/hash
 hash/root
 hash/construct)

;; A persistent hash table is a four element vector v:
;; v[0] - the literal symbol hash
;; v[1] - the key equality test, one of eq?, eqv?, or equal?
;; v[2] - the key hash function, one of eq-hash-code, eqv-hash-code, or equal-hash-code
;; v[3] - the top level trie, the root of the hash table.
(define-accessor hash/equality  1)
(define-accessor hash/hash      2)
(define-accessor hash/root      3)

;; Returns a new virgin hash table for Mischief.
(define (hash/construct equality? hasher root)
  (vector 'hashpersist equality? hasher root))

;; Type test for Mischief.
(define (hashpersist? h)
  (and
   (vector? h)
   (eq? 'hashpersist (vector-ref h 0))
   (= (vector-length h) 4)))

;; The constant empty hash tables that are the starting points for any persistent hash table instance.
(define hash/eq/null    (hash/construct eq?    eq-hash-code    trie/empty))
(define hash/eqv/null   (hash/construct eqv?   eqv-hash-code   trie/empty))
(define hash/equal/null (hash/construct equal? equal-hash-code trie/empty))

;; Return #t if h is an instance of an empty (null) hash table and #f otherwise.
(define (hash/null? h)
  (and (hashpersist? h) (eq? trie/empty (hash/root h))))

;; These three predicates detect the underlying equality test of hash table h.
(define (hash/eq? h)    (eq? (hash/equality h) eq?))
(define (hash/eqv? h)   (eq? (hash/equality h) eqv?))
(define (hash/equal? h) (eq? (hash/equality h) equal?))

;; User functions.

;; Nondestructively creates a fresh hash table h' that is the union of the contents of hash table
;; h and the key/value pairs (k_0 v_0 k_1 v_1 ...) given as input.
;; If h contains k_i/x for some value x then it will be replaced by k_i/v_i in h'.
(define (list/hash h input)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h)))
    (let loop ((input input)
               (t (hash/root h)))
      (if (null? input)
          (hash/construct equality? hasher t)
          (let ((key (car input))
                (value (cadr input)))
            (loop (cddr input) (trie/with equality? hasher t key value (hasher key) 0)))))))

;; (hash/new h k_0 v_0 ... k_i v_i) is equivalent to (list/hash h (k_0 v_0 ... k_i v_i)).
(define (hash/new h . rest)
  (list/hash h rest))

;; Nondestructively creates a fresh hash table h' that is the union of the contents of hash table
;; h and the the key value pairs ((k_0 . v_0) ... (k_i . v_i)) given as input.
;; Equivalent to (list/hash h k_0 v_0 ... k_i v_i).
(define (pairs/hash h input)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h)))
    (let loop ((pairs input)
               (t (hash/root h)))
      (if (null? pairs)
          (hash/construct equality? hasher t)
          (let ((pair (car pairs)))
            (loop (cdr pairs) (trie/with equality? hasher t (car pair) (cdr pair) (hasher (car pair)) 0)))))))

;; Fold function f with seed over all pairs appearing in hash table h.
(define (hash/fold h f seed)
  (pairs/fold f seed (hash/root h)))

;; Return the total number of key/value pairs in hash table h. 
(define (hash/length h)
  (pairs/count (hash/root h)))

(define (hash/empty? h)
  (eq? (hash/root h) trie/empty))

;; Return all key/value pairs in h as an association list ((k_0 . v_0) ...)
(define (hash/pairs h)
  (hash/fold h (lambda (pair seed) (cons pair seed)) null))

(define (hash/list h)
  (hash/fold h (lambda (pair seed) (cons (car pair) (cons (cdr pair) seed))) null))

;; Given hash table h return a successor hash table whose contents is the merge of h and the key/value pair.
;; If h contains key then that pair is replaced by the arguments in the successor.
(define (hash/cons h key value)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h)))
    (let ((t (trie/with equality? hasher (hash/root h) key value (hasher key) 0)))
      (hash/construct equality? hasher t))))

;; Return a successor to h whose contents are identical to h less the pair in h indexed by key.
(define (hash/remove h key)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h)))
    (let ((t (trie/without equality? (hash/root h) key (hasher key) 0)))
      (if (eq? t (hash/root h))
          h
          (hash/construct equality? hasher t)))))

;; Returns #t if hash table h contains key and #f otherwise.
(define (hash/contains? h key)
  (if (trie/get (hash/equality h) (hash/root h) key ((hash/hash h) key) 0)
      #t
      #f))
    
;; If hash table h contains key then return the value of the key/value pair otherwise return failure.
(define (hash/ref h key failure)
  (let ((pair (trie/get (hash/equality h) (hash/root h) key ((hash/hash h) key) 0)))
    (if pair
        (cdr pair)
        failure)))

;; Return the set of keys in hash table h as a list.
(define (hash/keys h)
  (hash/fold h (lambda (pair seed) (cons (car pair) seed)) null))

;; Return the set of values in hash table h as a list.
(define (hash/values h)
  (hash/fold h (lambda (pair seed) (cons (cdr pair) seed)) null))

;; Return the "first" key/value pair of hash table h.
;; hash/car is the hash table equivalent of car for lists.
(define (hash/car h)
  (let ((t (hash/root h)))
    (if (eq? trie/empty t)
        (error 'hash/car "car of empty hash")
        (trie/car t))))

;; Return the successor of hash table h in which the key/value pair returned by (hash/car h)
;; is absent in the successor. Equivalent to (hash/remove h (car (hash/car h))) but considerably
;; more efficient.
(define (hash/cdr h)
  (let ((t (hash/root h)))
    (if (eq? trie/empty t)
        (error 'hash/cdr "cdr of empty hash")
        (hash/construct (hash/equality h) (hash/hash h) (trie/cdr t)))))

;; Apply function f to each key/value pair in h and return a hash trie containing the results.
(define (hash/map h f)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h)))
    (define (map pair seed)
      (let ((x (f pair)))
        (if (pair? x) ; (key . value)
            (trie/with equality? hasher seed (car x) (cdr x) (hasher (car x)) 0)
            (error 'hash/map "pair required from function: ~s" x))))          
    
    (hash/construct
     equality?
     hasher
     (pairs/fold map trie/empty (hash/root h)))))

;; Return a hash table containing only those key/value pairs in h for which (p pair) is true.
(define (hash/filter h p)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h)))
    (define (filter pair seed)
      (if (p pair)
          (trie/with equality? hasher seed (car pair) (cdr pair) (hasher (car pair)) 0)
          seed)) ; Return the seed trie unchanged.
    
    (hash/construct
     (hash/equality h)
     (hash/hash h)
   (pairs/fold filter trie/empty (hash/root h)))))

;; Return a pair of hash tables (true . false) in which hash table true contains only those pairs of
;; hash table h for which predicate p returns #t and hash table false contains only those pairs of
;; h for which predicate p returns #f.
(define (hash/partition h p)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h)))
    ; The trick here is that seed is a two element vector for which element 0 is the root trie for the
    ; "true" partition and element 1 is the root trie for the "false" partition.
    (define (action pair seed)
      (let ((i (if (p pair) 0 1)))
        (vector-set!
         seed i
         (trie/with equality? hasher (vector-ref seed i) (car pair) (cdr pair) (hasher (car pair)) 0))
        seed))
    
    (let ((partition (pairs/fold action (vector trie/empty trie/empty) (hash/root h))))
      (cons
       (hash/construct equality? hasher (vector-ref partition 0))
       (hash/construct equality? hasher (vector-ref partition 1))))))
          

;; Return a hash table that is is the merge of beta INTO alpha. Any key/value pair in beta whose key
;; duplicates a key appearing in alpha will replace the corresponding key/value pair in alpha.
;; Consequently the length of the resulting hash table will be at most (+ (hash/length alpha) (hash/length beta)).
(define (hash/merge alpha beta)
  (let ((equality? (hash/equality alpha))
        (hasher    (hash/hash alpha)))
  (hash/construct
   equality?
   hasher
   (pairs/fold
    (lambda (pair seed)
      (trie/with equality? hasher seed (car pair) (cdr pair) (hasher (car pair)) 0))
    (hash/root alpha)    ; Seed.
    (hash/root beta))))) ; Source of pairs.

;; unfold p f g seed tail -> list
;; unfold is best described by its basic recursion:
;(define (hash/unfold h stop f g seed)
;  (if (stop seed)
;      h
;      (let ((pair (f seed)))
;        (hash/cons (hash/unfold h stop f g (g seed)) (car pair) (cdr pair)))))
;        

;; Ephemera for testing.

(define h/26
  (list/hash
   hash/eq/null
   '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
     k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
     u 21 v 22 w 23 x 24 y 25 z 26)))
;
;(define list/100
;    (let loop ((i 0)
;             (outcome null))
;    (if (< i 100)
;        (loop (add1 i) (cons i (cons i outcome)))
;        outcome)))
;
;(define h/100 (list/hash hash/eqv/null list/100))
;
;(define list/1000
;  (let loop ((i 0)
;             (outcome null))
;    (if (< i 1000)
;        (loop (add1 i) (cons i (cons i outcome)))
;        outcome)))
;
;(define h/1000 (list/hash hash/eq/null list/1000))
;
;(define (symbols/sort symbols)
;  (sort
;   symbols
;   (lambda (alpha beta) (string<? (symbol->string alpha) (symbol->string beta)))))
;
;(define (hash/car/test h)
;  (let loop ((h h)
;             (outcome null))
;    (if (hash/empty? h)
;        outcome
;        (loop (hash/cdr h) (cons (hash/car h) outcome)))))
