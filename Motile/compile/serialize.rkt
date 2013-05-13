#lang racket/base

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

;; This implementation borrows heavily from Racket v5.0/collects/racket/private/serialize.rkt
;; with numerous modifications to accommodate the serialized representations of Motile mobile
;; closures, continuations, and binding environments.
;; Many of the comments are paraphrases of the Racket Reference, Section 12.11, "Serialization."

;; Note: This code will be substantially rewritten in the future (post 2011-11) to remove the
;; serialization code devoted to the Racket-specific implementation of hash tables and to fully
;; convert the serialized representation from the use of lists to using vectors (for the sake
;; of memory efficiency and reducing garbage collection overhead).

(require
 data/heap
 (only-in file/sha1 sha1-bytes) ; Temporary until we get NaCl fully integrated.
 
 (only-in "../generate/baseline.rkt" motile/decompile)
 (only-in "recompile.rkt" motile/recompile motile/recompile/active?)

 (only-in "../persistent/ordered_trie.rkt" trie/pair? trie/flat/fold trie/pair-key trie/pair-value trie/pair-order)

 (only-in
  "../persistent/hash.rkt"
  ; Required for deconstruction and reconstruction.
  hash/persist?
  hash/eq?
  hash/eqv?
  hash/length
  hash/equality
  hash/eq/null
  hash/eqv/null
  hash/equal/null
  hash/hash
  hash/root
  ;hash/construct
  ;hash=>vector
  hash/insertion=>vector
  vector/hash)
 
 (only-in
  "../persistent/set.rkt"
  ; Required for deconstruction and reconstruction
  set/persist?
  set/eq?
  set/eqv?
  set/length
  set/equality
  set/eq/null
  set/eqv/null
  set/equal/null
  set/hash
  set/root
  ;set/construct
  set/vector
  vector/set)
 
 (only-in "../persistent/vector.rkt" vector/persist?)
 
 (only-in "../actor/actor.rkt" actor?)
 (only-in "../actor/jumpstart.rkt" actor/dead?)
  
 (only-in
  "../actor/locative.rkt"
  locative?
  locative/actor
  locative/expired? locative/unexpired?
  locative/expires
  locative/id
  locative/revoked
  locative/sends/positive?)
 
 (only-in
  "../actor/curl.rkt"
  curl? curl/ok?
  curl/export
  curl/id
  curl/id!
  curl/island
  curl/pretty curl/signing curl/signing!)

 (only-in
  "../actor/island.rkt"
  this/island
  island/address/equal?)
)
 
(provide 
 ; Checks whether a value is serializable:
 motile/serializable?
 ; The two main routines:
 motile/serialize motile/deserialize
 ; Equality test for serializations.
 motile/serialized/equal?
 ; Version of serialization format/implementation.
 motile/serialize/version
 export/statistics)

(define (ahead duration) (+ (current-inexact-milliseconds) duration))

(define EXPORTS (make-hasheq)) ; Map of locative ids to locatives for those locatives referenced by serialized (exported) CURLs.
(define DURABLES (make-hash))  ; Map of durable ids to locatives for durable CURLs.
(define EXPIRATIONS (make-heap (lambda (x y) (<= (locative/expires x) (locative/expires y))))) ; Priority queue ordered by locative expiration time.
(define REAPINGS 0) ; Total number of reapings to date.

(define (motile/serialize/version) '(1 0 0)) ; major/minor/patch

(define (export/statistics)
  (vector-immutable
   (hash-count DURABLES)
   (hash-count EXPORTS)
   (heap-count EXPIRATIONS)
   REAPINGS))

(define (thunk/reaper exports durables expirations)
  ; Return the first locative due to expire or #f if the priority queue is empty.
  (define (candidate expirations)
    (and (positive? (heap-count expirations)) (heap-min expirations)))

  (lambda ()
    (let loop ((timeout 10) ; Run the reaper every 10 seconds at a minimum.
               (receive (thread-receive-evt)))
      (let ((e (sync/timeout timeout receive)))
        (if e
            ; Got a message.
            (let enroll ((x (thread-receive)))
              (cond 
                ((and x (locative? x) (< (locative/expires x) +inf.0)) ; Don't bother with locatives whose lifespan is infinite.
                 (heap-add! expirations x)      ; Add locative x to the expirations heap.
                 (enroll (thread-try-receive))) ; Look for another locative to enroll in the expirations heap.
                (else (loop timeout receive)))) ; No more messages.
            
            ; Timeout. Reap as many expired locatives as possible.
            (let reap ((x (candidate expirations))) ; The locative x with the earliest expiration (if any).
              ; When the earliest locative has actually expired expunge it from the map of all exported (serialized) locatives.
              (when (and x (locative/expired? x))
                (heap-remove-min! expirations) ; Removee locative x from the priority heap.
                (let ((id (locative/id x)))
                  (and
                   (or (symbol? id) (pair? id))
                   (hash-remove! (if (symbol? id) exports durables) id)) ; Expunge locative x from the set of locatives referenced by serialized CURLs.
                (set! REAPINGS (add1 REAPINGS)) ; Total number of locatives reaped to date.
                (reap (candidate expirations)))) ; Try the next expiration.
              (loop timeout receive)))))))

;; Start the reaper thread in the background.
(define REAPER (thread (thunk/reaper EXPORTS DURABLES EXPIRATIONS)))

;; A serialized representation of a value v is a list
;; (<version> <structure-count> <structure-types> <n> <graph> <fixups> <final>) where:
;; <version> Version identifier of the representation given as a list (m) where m = 1, 2, ... is the version number
;; <structure-count> Unused by Mischief and always 0.
;; <structure-types> Unused by Mischief and always ().
;; <n> Length of the list <graph>
;; <graph> List of graph points. Each graph point is a serialized (sub)value to be used in the reconstruction of value v.
;; <fixups> List of pairs ((i . <serial>) ...) where each i = 0, 1, ... is an index into <graph> and <serial> is a specifier
;;          for the reconstruction of the value whose shape is given in graph point i.
;; <final> Serial specifier for the value v 

;; Graph points are either boxes whose contents specify the shape of a value to be constructed later:
;; [Note: #&(...) is the reader form for a box]
;;   #&(v . N) denotes a vector N elements wide
;;   #&(b) denotes a box
;;   #&(h) denotes a hash table with eq? keys
;;   #&(h weak) denotes a hash table with weak eq? keys
;;   #&(h equal) denotes a hash table with equal? keys
;;   #&(h equal weak) denotes a hash table with weak equal? keys
;; OR values to be constructed immediately:
;;   boolean, number, character, interned symbol, or empty list, representing itself
;;   a string "..." representing an immutable character string
;;   a bytes sequence #"..." representing an immutable bytes sequence
;;   (? . i) at position j > i in the graph representing the value constructed for the i'th graph point within the graph
;;   (void) representing the value #<void>
;;   (u . "...") representing the mutable character string "..."
;;   (u . #"...") representing the mutable bytes sequence #"..."
;;   (c X Y), X, Y themselves representations,
;;      an immutable pair (x . y) where x, y is the value represented by X (Y) respectively
;;   (v X_0 ... X_n), X_0 ... X_n themselves representations, denotes
;;      an immutable vector #(x_0 ... x_n) where x_i is the value represented by X_i
;;   (v! X_0 ... X_n), X_0 ... X_n themselves representations, denotes
;;      a mutable vector #(x_0 ... x_n) where x_i is the value represented by X_i
;;   (b X), X itself a representation, denotes an immutable box #&x where x is the value represented by X
;;   (b! X), X itself a representation, denotes a mutable box #&x where x is the value represented by X
;;   (h [!-] (K . X) ...) where ! (-) represents a mutable (immutable) hash table respectively,
;;      with eq? keys and each (K . V), K and V themselves representations, is a key/value pair k/v where
;;      k (v) is the value represented by K (V) respectively
;;   (h [!-] equal (K . V) ...)
;;   (h [!-] weak  (K . V) ...)
;;   (h [!-] equal weak (K . V) ...) represent hash tables with equal?, weak, and equal? weak keys respectively and whose
;;      contents are the key/value pairs k/v represented by K/V.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; serialize
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (motile/serializable? v)
  (or 
   ; Primitive values.
   (boolean? v)
   (null? v)
   (number? v)
   (char? v)
   (symbol? v)
   (string? v)
   (bytes? v)

   ; Basic persistent functional data structures.
   (vector/persist? v) ; Persistent functional vector.
   (hash/persist? v)   ; Persistent functional hash table.
   (set/persist? v)    ; Persistent functional set.

   ; Capability URLs (CURLs).
   (curl? v)

   ; Fundamental structured values.
   (vector? v) ; Covers Motile tuples as well since all tuples are vectors.
   (pair? v)
   (hash? v)   ; To be removed at some later date.
   (box? v)

   (void? v)
   (procedure? v)))

(define (mutable? o)
  (and
   (or 
    (box? o)
    (vector? o)
    (hash? o))
   (not (immutable? o))))

(define (for/each/vector v f)
  (let loop ((i 0)
             (n (vector-length v)))
    (when (< i n)
      (f (vector-ref v i))
      (loop (add1 i) n))))

;; Find a mutable object among those that contained in the current cycle.
(define (find-mutable v cycle/stack) 
  ; Walk back through cycle/stack to find something mutable.
  ; If we get to v without anything being mutable, then we're stuck.
  (let ([o (car cycle/stack)])
    (cond
      [(eq? o v)
       (error 'serialize "cannot serialize cycle of immutable values: ~e" v)]
      [(mutable? o) o]
      [else
       (find-mutable v (cdr cycle/stack))])))

(define (share-id share cycle)
  (+ (hash-count share)
     (hash-count cycle)))

;; Traverse v depth-first to identify cycles and sharing. Shared
;; object go in the `share' table, and mutable objects that are
;; cycle-breakers go in `cycle' table.
;; In each case, the object is mapped to a number that is
;; incremented as shared/cycle objects are discovered, so
;; when the objects are deserialized, build them in reverse
;; order.
(define (find-cycles-and-sharing v cycle share)
  (let ([cycle/candidates (make-hasheq)]  ;; Candidates for sharing
        [share/candidates (make-hasheq)]  ;; Candidates for cycles
        [cycle/stack null])               ;; Candidates for cycles but for finding mutables.
    (let loop ([v v])
      (cond
        [(or (boolean? v)
             (number? v)
             (char? v)
             (symbol? v)
             (null? v)
             ; Since the locative inside a CURL is never itself serialized it can be ignored for
             ; the purposes of sharing.
             (locative? v)
             (void? v))
         (void)]

        [(hash-ref cycle v #f)
         ; We already know that this value is part of a cycle so ignore.
         (void)]

        [(hash-ref cycle/candidates v #f)
         ; We have seen this object before in our depth-first search hence it must be a member of a cycle.
         (let ([v/mutable
                (if (mutable? v) v (find-mutable v cycle/stack))])
           ; v/mutable will be used to break the cycle.
           (hash-set! cycle v/mutable (share-id share cycle))
           (unless (eq? v/mutable v)
             ;; As v and v/mutable are distinct v is potentially shared (refered to) by other objects.
             (hash-set! share v (share-id share cycle))))]

        [(hash-ref share v #f)
         ;; We already know that this value is shared so ignore.
         (void)]

        [(hash-ref share/candidates v #f)
         ; We've just learned that v is shared (refered to) by at least two other objects.
         (hash-set! share v (share-id share cycle))]

        [else
         (hash-set! share/candidates v #t)
         (hash-set! cycle/candidates v #t)
         (set! cycle/stack (cons v cycle/stack))
         ; Now deconstruct v and account for sharing (if any) of its substructures.
         (cond
           [(or (string? v) (bytes? v))
            (void)] ; No sub-structure.

           ; Persistent hash tables require particular care as the Motile representation #('<hash/persist> <equality> <hash> <trie>)
           ; contains two Racket (not Motile!) procedures, <equality> and <hash>, the key equality test and the key hash code
           ; generator respectively. We don't want the serializer to see either of those as it has no idea of what to do with them.
           ; The only element that requires deep inspection is the trie representing the contents of the persistent hash table.
           ;[(hash/persist? v) (loop (hash/root v))]
           [(hash/persist? v)
            (trie/flat/fold
             (lambda (k v _seed) (loop k) (loop v)) #f (hash/root v))]

           ; Persistent sets require equal care as the Mischief representation is, with the exception of the
           ; tag in element 0, identical to that for persistent hash tables.
           [(set/persist? v) (loop (set/root v))]
           
           ; !!!!! FIX THIS !!!!!
           ;[(curl? v) (for/each/vector 

            ; Accounts for tuples as well since all tuples are just vectors with a type tag in element 0.
           [(vector? v)
            ;(for-each loop (vector->list v))]
            (for/each/vector v loop)] ; Experimental. Will eliminating the conversion of vector to list speed things up?

           [(pair? v)
            (loop (car v)) 
            (loop (cdr v))]

           [(box? v)
            (loop (unbox v))]

           [(hash? v)
            (hash-for-each v (lambda (k v) (loop k) (loop v)))]
           
           [(procedure? v)
            (let ((descriptor (motile/decompile v))) ; All Motile code/procedure/continuation descriptors are vectors.
              (loop descriptor))]

           [else (raise-type-error
                  'serialize
                  "serializable object"
                  v)])
         ; No more opportunities for this object to appear in a cycle as the depth-first search has returned.
         (hash-remove! cycle/candidates v)
         (set! cycle/stack (cdr cycle/stack))]))))

;; A temporary stub for CURL signing.
;; It uses the wrong SHA function and does not sign with the private key of the island.
;; This will all be fixed when we get the NaCL library fully integrated and begin using public keys
;; as globally unique island identifiers.
;; For now returns the SHA-1 hash of the byte string giving the motile/serialize representation of the CURL c.
;; c - a local CURL in export format, that is, (curl/id c) is NOT a locative but either a symbol (locative UUID)
;;     or a vector of symbols.
(define (curl/SHA/sign! c)
  (let ((out (open-output-bytes)))
    ; Paranoia.
    (curl/signing! c #f)
    ; Set the Motile type tag to something the serializer doesn't recognize as a special case
    ; otherwise the serializer will go into an infinite recursive loop.
    (vector-set! c 0 '<curl-for-signing>)
    ; Obtain the serialization as a byte string.
    (write (motile/serialize c) out)
    ; Generate a SHA hash of the the serialization and sign the real CURL
    (curl/signing! c (sha1-bytes (open-input-bytes (get-output-bytes out))))
    ; Patch the curl back up again.
    (vector-set! c 0 '<curl>)))

;; c - a deserialized, well-formed CURL with a byte string signature
;; Return #t if the CURL signature is valid and #f otherwise.
;; This routine is a temporary stub for the validation that uses the island private key.
;; Note that if c is validated then it is effectively resigned.
(define (curl/signing/validate c)
  (let ((signature (curl/signing c))) ; Save the CURL signature.
    (curl/signing! c #f)
    (curl/SHA/sign! c) ; Sign the CURL again.
    (bytes=? signature (curl/signing c)))) ; Compare our signing with the signature of the deserialized CURL.
    
;; Generate the serialization description (known as a "serial") for the given object v.
;; v: an object for which we require a "serial"
;; share: a hash table of shared objects (objects refered to by two or more objects)
;; share?: #t if the share table should be consulted in constructing the "serial" and #f otherwise
(define (serialize-one v share share?)
  ; Return the "serial" descriptor of object v.
  (define (serial v share?)
    (cond
      [(or (boolean? v)
           (number? v)
           (char? v)
           (null? v))
       v] ; The "serial" of a boolean, number, char or null constant is itself.
      
      [(symbol? v) v] ; The "serial" of a symbol is itself.
      
      [(void? v) ; The "serial" of #<void> is (void).
       '(void)]
      
      [(and share? (hash-ref share v #f))
       => (lambda (id) (cons '? id))] ; The "serial" of a shared object is (? . id) where id is a share id.
      
      [(and (or (string? v) (bytes? v))
            (immutable? v))
       v] ; The "serial" of an immutable character string or bytes sequence is itself.
      
      [(or (string? v) (bytes? v))
       (cons 'u v)] ; The "serial" of a mutable character string or bytes sequence v is (u . v)
      
      [(vector/persist? v)
       ; A persistent vector v has the form #('<vector/persist> <count> <shift> <root> <tail>) where
       ; <count> and <shift> are integers >= 0,
       ; <root> is an ordinary mutable vector that is the root of the trie representing the persistent vector and
       ; <tail> is an ordinary mutable vector (of at most 32 elements) representing the current tail of the persistent vector.
       ; The serial representation is (V '<vector/persist> <count> <shift> <r> <t>) where <r> and <t> are the serial representations
       ; of the root and tail respectively.
       (cons 'V (map (lambda (x) (serial x #t)) (vector->list v)))]

      [(hash/persist? v)
       ; A persistent hash table v has the form #('<hash/persist> <equality> <hash> <root>) where:
       ;    <equality> and <hash> are Racket procedures for testing key equality and generating key hash codes respectively and
       ;    <root> is the root of the trie representing the hash table.
       (vector ;(list
        'H
        (cond
          ((hash/eq?  v) 'eq)
          ((hash/eqv? v) 'eqv)
          (else          'equal))
        ;(serial (hash/root v) #t))]
        (serial (hash/insertion=>vector v) #t))]

      [(set/persist? v)
       ; A persistent set v has the form #('<set/persist> <equality> <hash> <root>) where:
       ;    <equality> and <hash> are the Racket procedures for testing set member equality and
       ;      generating member hash codes respectively and
       ;   <root> is the root of the trie representing the set.
       (list
        'S
        (cond
          ((set/eq?  v) 'eq)
          ((set/eqv? v) 'eqv)
          (else         'equal))
        ;(serial (set/root v) #t))]
        (serial (set/vector v) #t))]

      [(curl? v)
       ;(log-info (format "motile/serialize: saw curl: ~a\n\n" (curl/pretty v)))
       (cons
        'C
        (map
         (lambda (e) (serial e #t)) ; Serialize each element of the CURL in turn.
         (vector->list
          (if (locative? (curl/id v))
              ; v must be a intra-island CURL so build a signed version for serialization.
              ; It isn't clear if we should concern ourselves here with the possibility that the locative is exhausted,
              ; has been revoked, or has expired.
              (let* ((locative (curl/id v))
                     (lid      (locative/id locative))) ; Locative ID.
                (if (and
                     (not (locative/revoked locative))
                     (locative/sends/positive? locative)
                     (locative/unexpired? locative)
                     (or (symbol? lid) (pair? lid))
                     (not (actor/dead? (locative/actor locative))))

                    (let* ((x (curl/export v))) ; In this case x is guaranteed to be a copy of v.
                      (curl/SHA/sign! x)
                      (let ((map (if (symbol? lid) EXPORTS DURABLES)))
                        (unless (hash-ref map lid #f)
                          (hash-set! map lid locative)
                          (thread-send REAPER locative)))
                      x)

                    (error 'motile/serialize (format "curl: ~a locative is expired, exhausted, revoked, or dead" (curl/pretty v)))))

              ; v must be an inter-island CURL and hence signed by the issuing island.
              ; Should we think about validating the signing ourselves before serializing it?
              v))))]

      [(vector? v)
       ; "serial" for immutable vector x is (v s_0 ... s_N)  where s_i is the serial of element i of x.
       ; "serial" for mutable vector   x is (v! s_0 ... s_N) where s_i is the serial of element i of x.
       (cons (if (immutable? v) 'v 'v!)
             (map (lambda (x) (serial x #t)) (vector->list v)))]
      
      [(pair? v)
       ; The "serial" of a pair (x . y) is (c X . Y) where X, Y is the "serial" of x, y respectively. 
       (cons 'c
             (cons (serial (car v) #t)
                   (serial (cdr v) #t)))]
      
      [(box? v)
       ; The "serial" of an immutable box is (b . X) where X is the "serial" of the contents of box v.
       (cons (if (immutable? v) 'b 'b!) (serial (unbox v) #t))]
      
      [(procedure? v)
       ;(cons 'M (serial (v #f #f) #t))]
       (cons 'M (serial (motile/decompile v) #t))]

      [(hash? v)
       ; The "serial" of a hash table is (h <mutable> <modifiers> (X_1 . Y_1) ... (X_N . Y_N) where:
       ; <mutable> denotes an immutable - or mutable ! hash table
       ; <modifiers> is one of (), (equal), (weak), or (equal weak)
       ;  signifying eq?, equal?, eq? and weak, equal? and weak keys respectively.
       ; X_i, Y_i is the serial of key k_i and its value v_i respectively.
       (list* 'h
              (if (immutable? v) '- '!)
              (append
               (if (not (hash-eq? v)) '(equal) null)
               (if (hash-weak? v) '(weak) null))
              (hash-map v (lambda (k v) (cons (serial k #t) (serial v #t)))))]
      
      [else (error 'serialize "shouldn't get here")]))
  
  (serial v share?))

;; Return the encoding for a cyclic graph point for value v.
;; Only a vector, box, or hash table may contribute to a cycle.
(define (serial-shell v)
  (cond
    [(vector? v)
     (cons 'v (vector-length v))]
    [(box? v)
     'b]
    [(hash? v)
     (cons 'h (append
               (if (not (hash-eq? v)) '(equal) null)
               (if (hash-weak? v) '(weak) null)))])) 

(define (motile/serialize v)
  (let ([share (make-hasheq)]
        [cycle (make-hasheq)])
    ; Traverse v to find cycles and sharing
    (find-cycles-and-sharing v cycle share)
    ;; To simplify, add all of the cycle records to shared (but retain the cycle information).
    (hash-for-each cycle (lambda (k v) (hash-set! share k v)))
    
    (let ([ordered ; List of all shared and cycle-breaking objects in ascending order by their respective share id.
           (map car
                (sort
                 (hash-map share cons) ; An association list ((o . id) ...) of object o with share id.
                 (lambda (a b) (< (cdr a) (cdr b)))))]) ; Sort by ascending share id.
      
      (let ([serializeds ; Issued in order of their appearance in the depth-first tour of v.
             (map
              (lambda (v)
                (if (hash-ref cycle v #f)
                    ; Box indicates cycle record allocation followed by normal serialization
                    (box (serial-shell v))
                    ; Otherwise, normal serialization
                    (serialize-one v share #f)))
              ordered)]

            [fixups ; All cycle-breaker serializations as an association list ((id . s) ...) where id is the serial id.
             (hash-map 
              cycle
              (lambda (v n) (cons n (serialize-one v share #f))))]
            
            [final (serialize-one v share #t)])
        
        (vector
         (motile/serialize/version) ; Version of serialization format.
         (length serializeds)       ; Total number of shared objects.
         serializeds                ; Serialized objects in order of depth-first tour.
         fixups
         final)))))

;        (list '(2) ;; serialization-format version
;              0    ; Number of distinct structure types. Unused in Motile and just temporary.
;              null ; List of distinct structure types. Unused in Motile and just temporary.
;              (length serializeds)
;              serializeds ; The graph structure of the value.
;              fixups
;              final)))))

(define (flat/version x) (vector-ref x 0)) ; Version number of serialization format.
(define (flat/total   x) (vector-ref x 1)) ; Total number of serialized objects.
(define (flat/objects x) (vector-ref x 2)) ; The serialized objects.
(define (flat/fixups  x) (vector-ref x 3)) ; Patches to account for shared objects.
(define (flat/final   x) (vector-ref x 4)) ; The "final" object, that is, the object that was handed to motile/serialize.

(define (flat/ok? x)
  (and
   (vector? x)
   (= (vector-length x) 5)
   (list? (flat/version x))
   (exact-nonnegative-integer? (flat/total x))))

        (define (extract-version l)
  (if (pair? (car l))
      (values (caar l) (cdr l)) ; Values are <version> and (<unused_1> null <serializeds/length> <serializeds> <fixups> <final>) respectively.
      (values 0 l)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deserialize
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-hash/flags v)
  (cond
    [(null? v) (make-hasheq)]
    [(eq? (car v) 'equal)
     (if (null? (cdr v))
         (make-hash)
         (make-weak-hash))]
    [else (make-weak-hasheq)]))

(define-struct not-ready (shares fixup))

(define (lookup-shared! share n procedures)
  ;; The shared list is not necessarily in order of refereds before referees. A `not-ready' object
  ;;  indicates a reference before a value is ready,
  ;;  so we need to recur to make it ready. Cycles
  ;;  have been broken, though, so we don't run into
  ;;  trouble with an infinite loop here.
  (let ([value/shared (vector-ref share n)])
    (if (not-ready? value/shared)
        ; Reconstruct the shared value.
        (let* ([x (vector-ref (not-ready-shares value/shared) n)] ; x is shares[n]
               [reconstruction
                (if (box? x)
                    (deserial-shell (unbox x) (not-ready-fixup value/shared) n)
                    (deserialize-one x share procedures))])
          (vector-set! share n reconstruction)
          reconstruction)

        value/shared)))

(define (deserialize-one v share procedures)
  (let loop ([v v])
    (cond
      [(or (boolean? v)
           (number? v)
           (char? v)
           (symbol? v)
           (null? v))
       v]

      [(string? v) ; A standalone character string "..." denotes the equivalent immutable character string.
       (string->immutable-string v)]

      [(bytes? v)  ; A standalone bytes sequence #"..." denotes the equivalent immutable bytes sequence.
       (bytes->immutable-bytes v)]

      [else
       (case (if (vector? v) (vector-ref v 0) (car v))
         [(?) (lookup-shared! share (cdr v) procedures)] ; (? . i) where i is index into share

         [(void) (void)]                      ; (void)

         [(u) (let ([x (cdr v)])              ; (u . "...") or (u . #"..."). A mutable character string or bytes sequence respectively.
                (cond
                  [(string? x) (string-copy x)]
                  [(bytes? x) (bytes-copy x)]))]

         [(c) (cons (loop (cadr v)) (loop (cddr v)))]       ; (c x y). Pair (x . y).

         [(v) (apply vector-immutable (map loop (cdr v)))]  ; (v e_0 ... e_N). Immutable vector #(e_0 ... e_N).
         
         [(v!) (list->vector (map loop (cdr v)))]           ; (v! e_0 ... e_N). Mutable vector #(e_0 ... e_N).

         [(V) (list->vector (map loop (cdr v)))]            ; (V '<vector/persist> <count> <shift> <r> <t>) =>
                                                            ;     persistent vector #('<vector/persist> <count> <shift> <root> <tail>.

;         [(H) ; (H <equality> <trie>). Persistent hash table.
;          (let-values
;              ([(equality hasher)
;                 (case (cadr v)
;                   ((eq)    (values eq?    eq-hash-code))
;                   ((eqv)   (values eqv?   eqv-hash-code))
;                   ((equal) (values equal? equal-hash-code)))])
;            (hash/construct equality hasher (loop (caddr v))))]
         [(H) ; #(H <equality> <key/value vector>).  (H <equality> <key/value vector>)
          (let ((contents (vector-ref v 2)) ;(caddr v))
                (equality (vector-ref v 1))) ;(cadr v)))
            (vector/hash
             (case equality
               ((eq)    hash/eq/null)
               ((eqv)   hash/eqv/null)
               ((equal) hash/equal/null))
             (loop contents)))]

         [(S) ; (S <equality> <trie>). Persistent set.
;          (let-values
;              ([(equality hasher)
;                 (case (cadr v)
;                   ((eq)    (values eq?    eq-hash-code))
;                   ((eqv)   (values eqv?   eqv-hash-code))
;                   ((equal) (values equal? equal-hash-code)))])
;            (set/construct equality hasher (loop (caddr v))))]
          ; (S <equality> <elements vector>)
          (let ((contents (caddr v))
                (equality (cadr v)))
            (vector/set
             (case equality
               ((eq)    set/eq/null)
               ((eqv)   set/eqv/null)
               ((equal) set/equal/null))
             (loop contents)))]

         [(C) ; CURL.
          ;(log-info (format "deserialize-one: saw CURL ~a\n\n" v))
          (let ((c (list->vector (map loop (cdr v)))))
            ;(log-info (format "post-map CURL is ~a\n\n" c))
            (if (curl/ok? c #t) ; #t => check for presence of signature (but not verification).
                (if (island/address/equal? (this/island) (curl/island c))
                    ; CURL c claims to have originated on this island. Validate its signature.
                    (if (curl/signing/validate c)
                        ; Yes, CURL c originated here. Replace its curl/id with the correct locative.
                        (let* ((id (curl/id c)) ; curl/id = UUID or an arbitrary non-empty list   ; OLD CODE; (vector-ref (curl/id c) 0)) 
                               (locative (hash-ref (if (symbol? id) EXPORTS DURABLES) id #f))) ; Find the intra-island locative in the EXPORTS map.
                          (cond
                            (locative (curl/id! c locative))
                            (else
                             (curl/id! c #f)        ; #f replaces the original (but expired) locative.
                             (curl/signing! c #f))) ; Smash the signing since the signature is no longer valid.
                          c)

                        ; CURL c is a forgery.
                        (raise-type-error 'motile/deserialize "forged curl" c))

                    ; CURL c originated on another island and is well-formed so just return it.
                    c)

                ; CURL c is ill-formed. Reject it.
                (raise-type-error 'motile/deserialize "curl" c)))]

         [(b) (box-immutable (loop (cdr v)))]               ; (b . x). Immutable box #&(x).

         [(b!) (box (loop (cdr v)))]                        ; (b! . x). Mutable box #&(x).

         ; (h [!-] ([equal][weak]) ((x . y) ...) where x is a serial id for a key and y a serial id for a value.
         [(h) (let ([al (map (lambda (p)                    
                               (cons (loop (car p))
                                     (loop (cdr p))))
                             (cdddr v))])
                (if (eq? '! (cadr v))
                    (let ([ht (make-hash/flags (caddr v))])
                      (for-each (lambda (p)
                                  (hash-set! ht (car p) (cdr p)))
                                al)
                      ht)
                    (if (null? (caddr v))
                        (make-immutable-hasheq al)
                        (make-immutable-hash al))))]

         [(M) ; Motile code descriptor.
          (let* ((descriptor (loop (cdr v)))
                 (code (motile/recompile descriptor)))
            (when (motile/recompile/active? descriptor) (hash-set! procedures code descriptor))
;            (when (or 
;                   (lambda/inner?       descriptor)
;                   (closure/inner?      descriptor)
;                   (lambda/rest/inner?  descriptor)
;                   (closure/rest/inner? descriptor))
;              (hash-set! procedures code descriptor))
            code)]

         [else (raise-type-error 'motile/deserialize "<unknown serialized object>" (car v))])])))

(define (deserial-shell v fixup n)
  (cond
    [(pair? v)
     (case (car v)
       [(v)
        ; Vector 
        (let* ([m (cdr v)] ; Size of reconstructed vector.
               [reconstruction (make-vector m #f)])
          (vector-set!
           fixup
           n
           (lambda (v)
             (let loop ((i (sub1 m)))
               (unless (< i 0)
                 (vector-set! reconstruction i (vector-ref v i))
                 (loop (sub1 i))))))
          reconstruction)]             
             
;             (let loop ([i m])
;               (unless (zero? i)
;                 (let ([i (sub1 i)])
;                   (vector-set! reconstruction i (vector-ref v i))
;                   (loop i))))))

       [(h)
        ;; Hash table
        (let ([reconstruction (make-hash/flags (cdr v))])
          (vector-set!
           fixup n
           (lambda (h)
             (hash-for-each h (lambda (k v) (hash-set! reconstruction k v)))))
          reconstruction)])]

    [else
     (case v
       [(c)
        (let ([c (cons #f #f)])
          (vector-set! fixup n (lambda (p) (error 'deserialize "cannot restore pair in cycle")))
          c)]

       [(b)
        (let ([reconstruction (box #f)])
          (vector-set! fixup n (lambda (b) (set-box! reconstruction (unbox b))))
          reconstruction)])]))

(define (deserialize-with-map version flat procedures)
  (let ([share/n (flat/total flat)]
        [shares  (flat/objects flat)]
        [fixups  (flat/fixups flat)]
        [final   (flat/final flat)])
;  (let ([share/n (list-ref l 2)]  ; <serializeds/length>
;        [shares  (list-ref l 3)]  ; <serializeds>
;        [fixups  (list-ref l 4)]  ; The fixups, an association list of ((<id> . <serial>) ...) in ascending order of the share-id.
;        [final   (list-ref l 5)])
    ; Create vector for sharing:
    (let* ([fixup (make-vector share/n #f)]
           [unready (make-not-ready (list->vector shares) fixup)]
           [share (make-vector share/n unready)])

      ; Deserialize into sharing array:
      (let loop ([n 0] [l shares])
        (unless (= n share/n)
          (lookup-shared! share n procedures)
          (loop (add1 n) (cdr l))))

      ; Fixup shell for graphs
      (for-each
       (lambda (n+v)
         (let ([v (deserialize-one (cdr n+v) share procedures)]
               [fixer (vector-ref fixup (car n+v))])
           (fixer v)))
       fixups)

      ; Deserialize final result. (If there's no sharing, then all the work is actually here.)
      (deserialize-one final share procedures))))



;; Reconstitute the flat serialization into a live data structure closures and all.
;; flat: a serialization generate by (serialize ...)
;; globals: a Mischief global binding environment or #f. If #f then no closure embedded in flat will have its run-time stack
;;   properly reset and must be done, per-closure, prior to closure execution.
;; procedures?: If #f then return just the live data structure. If #t then return a pair (<live> . <procedures>) where
;;   <live> is the live data structure and <procedures> is an eq? hash table whose keys are the closures embedded in the live
;;   structure and whose values are the reconstituted closure descriptors embedded in the flat representation.
;;   The keys are effectively an enumeration of every closure contained in the live structure and the descriptors contain
;;   both the abstract assembly code for each closure as well as the the closed variables bindings for each closure.
;(define (motile/deserialize flat procedures?)
;  (let-values ([(version flat) (extract-version flat)])
;    ; At this point version is a small positive integer and flat = (<unused> null <serializeds/length> <serializeds> <fixups> <final>).
;    ; The first two elements of flat are historical artifacts and will be removed in a future version of serialize/deserialize.
;    (let* ((procedures (make-hasheq))
;           (outcome    (deserialize-with-map version flat procedures)))
;      (if procedures?
;          (cons outcome procedures)
;          outcome))))

(define (motile/deserialize flat procedures?)
  (if (flat/ok? flat)
      (let ((version (flat/version flat)))
        (if (equal? version (motile/serialize/version))
            (let* ((procedures (make-hasheq))
                   (outcome (deserialize-with-map version flat procedures)))
              (if procedures?
                  (cons outcome procedures)
                  outcome))
            (error 'motile/deserialize "unknown version")))
      (error 'motile/deserialize "unknown format")))

;(define (deserialize flat globals procedures?)
;  (let-values ([(version flat) (extract-version flat)])
;    ; At this point version is a small positive integer and flat = (<unused> null <serializeds/length> <serializeds> <fixups> <final>).
;    ; The first two elements of flat are historical artifacts and will be removed in a future version of serialize/deserialize.
;    (let* ((procedures (make-hasheq))
;           (outcome    (deserialize-with-map version flat procedures))
;           (frame      (and globals (vector #f globals)))
;           (reframe    (and frame (cons frame null))))
;      ; If there are Mischief procedures embedded somewhere in the deserialization then patch them up for execution.
;      (when (positive? (hash-count procedures))
;        (hash-for-each
;         procedures
;         (lambda (p d) ; p is the reconstituted Motile closure and d is the decompilation descriptor of that closure.
;           ; Reset the bindings of each closure to their values at the time of serialization.
;           (when (or (code/closure/inner? d) (code/closure/rest/inner? d))
;             ;(p #f (code/closure/inner/bindings d)))
;             ; Call the reconstituted Motile closure p as (p #f n b) where
;             ; #f - informs the closure that is either being decompiled or re
;             (p #f (code/closure/inner/bindings/span d) (code/closure/inner/bindings d)))
;           ; If a global binding environment is given then supply each lambda and closure with the proper base frame
;           ; (containing the global binding environment) for its run time stack.
;           (when globals
;             (p #f reframe)))))
;      (if procedures?
;          (cons outcome procedures)
;          outcome))))

;; ----------------------------------------

;(define (motile/serialized/equal? v l1 l2)
;  (let-values ([(version1 l1) (extract-version l1)]
;               [(version2 l2) (extract-version l2)])
;    (let ([v1 (deserialize-with-map version1 l1)]
;          [v2 (deserialize-with-map version2 l2)])
;      (equal? v1 v2))))

;; Test two serializations flat_1 and flat_2 for equality.
(define (motile/serialized/equal? flat_1 flat_2)
  (let ([v_1 (motile/deserialize flat_1 #f)]
        [v_2 (motile/deserialize flat_2 #f)])
    (equal? v_1 v_2)))




