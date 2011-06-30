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

(provide
 BASELINE
 ENVIRON/TEST)

(provide define/global/0
         define/global/1
         define/global/2
         define/global/3
         define/global/N
         define/combinator/2
         define/combinator/3)

(require
 racket/pretty
 "persistent/environ.rkt"
 "persistent/vector.rkt"
 "persistent/hash.rkt"
 "persistent/set.rkt"
 "persistent/tuple.rkt")

;; Many of the persistent functional data structures implement combinators of the form (P d f)
;; where C is a combinator of the persistent functional data structure, d is an instance
;; of the data structure, and f a function applied by the combinator.
;; These combinators must be wrapped before they can be used within Motile.
;; The function defined here generates that wrapping.
(define (motile/combinator/2 symbol combinator)
  (case-lambda
    ((rtk d f)
     (let ((g (lambda (x) (f k/RETURN x))))
       (rtk (combinator d g))))
    ((k _) (global/decompile k symbol))))

;; In the same vein many of the persistent functional data structures implement fold-like
;; combinators that require three arguments, a data structure instance d, a function f of two arguments
;; applied by the combinator, and a seed value.
;; As above, these combinators must be wrapped before they can be used within Motile.
;; The function defined here generates that wrapping.
(define (motile/combinator/3 symbol combinator)
  (case-lambda
    ((rtk d f x)
     (let ((g (lambda (x y) (f k/RETURN x y))))
       (rtk (combinator d g x))))
    ((k _) (global/decompile k symbol))))
  
;; The following functions wrap host Scheme primitives allowing those primitives to be invoked
;; within Motile and to be properly "decompiled" for network transmission.
;(define (global/0 symbol procedure)
;  (case-lambda 
;    ((rtk) (rtk (procedure)))
;    ((k x) (vector 'reference/global symbol))))

(define (motile/global/0 symbol procedure)
  (let ((descriptor (vector 'reference/global symbol)))
    (lambda (k _rte _global)
      (if k
          (k (procedure))
          descriptor))))

;(define (global/1 symbol procedure)
;  (lambda (rtk a) 
;    (if rtk
;        (rtk (procedure a))
;        (vector 'reference/global symbol))))

(define (motile/global/1 symbol procedure)
  (let ((descriptor (vector 'reference/global symbol)))
    (case-lambda
      ((k _rte _global a) (k (procedure a)))
      ((k _rte _global) (unless k descriptor)))))

;(define (global/2 symbol procedure)
;  (case-lambda 
;    ((rtk a b) (rtk (procedure a b)))
;    ((k x) (vector 'reference/global symbol))))

(define (motile/global/2 symbol procedure)
  (let ((descriptor (vector 'reference/global symbol)))
    (case-lambda
      ((k _rte _global a b) (k (procedure a b)))
      ((k _rte _global) (unless k descriptor)))))

;(define (global/3 symbol procedure)
;  (case-lambda 
;    ((rtk a b c) (rtk (procedure a b c)))
;    ((k x) (vector 'reference/global symbol))))

(define (motile/global/3 symbol procedure)
  (let ((descriptor (vector 'reference/global symbol)))
    (case-lambda
      ((k _rte _global a b c) (k (procedure a b c)))
      ((k _rte _global) (unless k descriptor)))))

(define (global/N symbol procedure)
  (lambda (rtk . arguments)
    (if rtk
        (rtk (apply procedure arguments))
        (vector 'reference/global symbol))))

(define (motile/global/N symbol procedure)
  (let ((descriptor (vector 'reference/global symbol)))
    (lambda (k _rte _global . arguments)
      (if k
          (k (apply procedure arguments))
          descriptor))))

;(define (global/0/generate symbol procedure)
;  (case-lambda 
;    ((rtk) (rtk (procedure)))
;    ((k x) (vector 'reference/global symbol))))
;
;(define (global/1/generate symbol procedure)
;  (lambda (rtk a) 
;    (if rtk
;        (rtk (procedure a))
;        (vector 'reference/global symbol))))
;
;(define (global/2/generate symbol procedure)
;  (case-lambda 
;    ((rtk a b) (rtk (procedure a b)))
;    ((k x) (vector 'reference/global symbol))))
;
;(define (global/3/generate symbol procedure)
;  (case-lambda 
;    ((rtk a b c) (rtk (procedure a b c)))
;    ((k x) (vector 'reference/global symbol))))
;
;(define (global/N/generate symbol procedure)
;  (lambda (rtk . arguments)
;    (if rtk
;        (rtk (apply procedure arguments))
;        (vector 'reference/global symbol))))

(define-syntax-rule (define/global/0 symbol procedure)
  (cons symbol (motile/global/0 symbol procedure)))

(define-syntax-rule (define/global/1 symbol procedure)
  (cons symbol (motile/global/1 symbol procedure)))

(define-syntax-rule (define/global/2 symbol procedure)
  (cons symbol (motile/global/2 symbol procedure)))

(define-syntax-rule (define/global/3 symbol procedure)
  (cons symbol (motile/global/3 symbol procedure)))

(define-syntax-rule (define/global/N symbol procedure)
  (cons symbol (motile/global/N symbol procedure)))

(define-syntax-rule (define/combinator/2 symbol combinator)
  (cons symbol (motile/combinator/2 symbol combinator)))

(define-syntax-rule (define/combinator/3 symbol combinator)
  (cons symbol (motile/combinator/3 symbol combinator)))

;; Special cases
;; Higher-order host primitives that accept a closure C as an argument, for example, apply or map,
;; require special treatment as a Motile closure requires a continuation k as its first argument.

(define (k/RETURN x) x) ; The trivial continuation.

;; Motile-specific reworkings of map, apply, and for-each that accomodates the closure argument correctly.
(define (motile/map rtk f . arguments)
  (if rtk
      (let ((g (lambda rest (apply f k/RETURN rest))))
        (rtk (apply map g arguments)))
      (vector 'reference/global 'map)))

(define (motile/apply rtk f . arguments)
  (if rtk
      (rtk (apply f k/RETURN arguments))
      (vector 'reference/global 'apply)))

(define (motile/for-each rtk f . arguments)
  (if rtk
      (let ((g (lambda rest (apply f k/RETURN rest))))
        (rtk (apply for-each g arguments)))
      (vector 'reference/global 'for-each)))

(define (global/decompile k name)
  (if k
      (error name "too few arguments")
      (vector 'reference/global name)))

(define motile/sort
  (case-lambda
    ((rtk items less?)
     (let ((p (lambda (alpha beta) (less? k/RETURN alpha beta))))
       (rtk (sort items p))))
    ((k _) (global/decompile k 'sort))))


;; Motile-specific reworkings of persistent vector primitives that accept functions as arguments.

;(define motile/vector/build
;  (case-lambda
;    ((rtk n f)
;     (let ((g (lambda (i) (f k/RETURN i))))
;        (rtk (vector/build n g))))
;    ((k _) (global/decompile k 'vector/build))))
;
;(define motile/vector/fold/left
;  (case-lambda
;    ((rtk v f seed)
;     (let ((g (lambda (value seed) (f k/RETURN value seed))))
;        (rtk (vector/fold/left v g seed))))
;    ((k _) (global/decompile k 'vector/fold/left))))
;
;(define motile/vector/fold/right
;  (case-lambda
;    ((rtk v f seed)
;     (let ((g (lambda (value seed) (f k/RETURN value seed))))
;        (rtk (vector/fold/right v g seed))))
;    ((k _) (global/decompile k 'vector/fold/right))))
;
;(define motile/vector/filter
;  (case-lambda
;    ((rtk v p)
;     (let ((q (lambda (value) (p k/RETURN value))))
;       (rtk (vector/filter v q))))
;    ((k _) (global/decompile k 'vector/filter))))
;
;(define motile/vector/map
;  (case-lambda
;    ((rtk v f)
;     (let ((g (lambda (value) (f k/RETURN value))))
;        (rtk (vector/map v g))))
;    ((k _) (global/decompile 'vector/map))))

;; Motile-specific reworkings of higher-order, persistent hash table primitives 

;(define motile/hash/fold
;  (case-lambda
;    ((rtk h f seed)
;     (let ((g (lambda (value seed) (f k/RETURN value seed))))
;        (rtk (hash/fold h g seed))))
;    ((k _) (global/decompile k 'hash/fold))))
;
;(define motile/hash/map
;  (case-lambda
;    ((rtk h f)
;     (let ((g (lambda (pair) (f k/RETURN pair))))
;        (rtk (hash/map h g))))
;    ((k _) (global/decompile k 'hash/map))))
;
;(define motile/hash/filter
;  (case-lambda
;    ((rtk h p)
;     (let ((q (lambda (pair) (p k/RETURN pair))))
;        (rtk (hash/filter h q))))
;    ((k _) (global/decompile k 'hash/filter))))
;
;(define motile/hash/partition
;  (case-lambda
;    ((rtk h p)
;     (let ((q (lambda (pair) (p k/RETURN pair))))
;        (rtk (hash/partition h q))))
;    ((k _) (global/decompile k 'hash/partition))))

;; Motile-specific reworkings of higher-order, persistent unordered set primitives.

;(define motile/set/fold
;  (case-lambda
;    ((rtk s f seed)
;     (let ((g (lambda (value seed) (f k/RETURN value seed))))
;        (rtk (set/fold s g seed))))
;    ((k _) (global/decompile k 'set/fold))))
;
;(define motile/set/map
;  (case-lambda
;    ((rtk s f)
;     (let ((g (lambda (value) (f k/RETURN value))))
;        (rtk (set/map s g))))
;    ((k _) (global/decompile k 'set/map))))
;
;(define motile/set/filter
;  (case-lambda
;    ((rtk s p)
;     (let ((q (lambda (value) (p k/RETURN value))))
;        (rtk (set/filter s q))))
;    ((k _) (global/decompile k 'set/filter))))
;
;(define motile/set/partition
;  (case-lambda
;    ((rtk s p)
;     (let ((q (lambda (value) (p k/RETURN value))))
;        (rtk (set/partition s q))))
;    ((k _) (global/decompile k 'set/partition))))


;; Motile-specific reworkings of higher-order tuple primitives.

;(define (motile/tuple/build)
;  (case-lambda
;    ((rtk n f)
;     (let ((g (lambda (index) (f k/RETURN index))))
;       (rtk (tuple/build n g))))
;    ((k _) (global/decompile k 'tuple/build))))
;
;(define(motile/tuple/filter)
;  (case-lambda
;    ((rtk t f)
;     (let ((g (lambda (x) (f k/RETURN x))))
;       (rtk (tuple/filter t g))))
;    ((k _) (global/decompile k 'tuple/filter))))
;
;(define (motile/tuple/map)
;  (case-lambda
;    ((rtk t f)
;     (let ((g (lambda (x) (f k/RETURN x))))
;       (rtk (tuple/map t g))))
;    ((k _) (global/decompile k 'tuple/map))))

;; Motile-specific call/cc.
(define (call/cc k f)
  (let ((descriptor (vector 'reference/global 'call/cc)))
    (if k
        (f k                           ; Continuation at call position of call/cc.
           (lambda (k/other v) (k v))) ; Continuation k reified as a function.
        descriptor)))

;; The set of primitive procedures available to all Motile mobile code.
(define BASELINE
  (pairs/environ
   environ/null
   (list
    ; Type testing.
    (define/global/1 'boolean?   boolean?)
    (define/global/1 'list?      list?)
    (define/global/1 'null?      null?)
    (define/global/1 'pair?      pair?)
    (define/global/1 'symbol?    symbol?)
    (define/global/1 'number?    number?)
    (define/global/1 'real?      real?)
    (define/global/1 'integer?   integer?)
    (define/global/1 'exact?     exact?)
    (define/global/1 'inexact?   inexact?)
    (define/global/1 'procedure? procedure?)
    
    ; Type transformers for numbers.
    (define/global/1 'exact->inexact exact->inexact)
    (define/global/1 'inexact->exact inexact->exact)
    (define/global/1 'number->string number->string)
    (define/global/1 'string->number string->number)
    
    ; Equivalence predicates.
    (define/global/2 'eqv?    eqv?)
    (define/global/2 'eq?     eq?)
    (define/global/2 'equal?  equal?)
    
    ; List test and manipulation.
    (define/global/1 'length length)
    (define/global/N 'append append)
    (define/global/1 'reverse reverse)
    
    ; List construction and deconstruction.
    (define/global/2 'cons     cons)
    (define/global/N 'list     list)
    (define/global/N 'list*    list*)
    (define/global/1 'car      car)
    (define/global/1 'cdr      cdr)
    (define/global/1 'caar     caar)
    (define/global/1 'cadr     cadr)
    (define/global/1 'cdar     cdar)
    (define/global/1 'cddr     cddr)
    (define/global/1 'caaar    caaar)
    (define/global/1 'caadr    caadr)
    (define/global/1 'cadar    cadar)
    (define/global/1 'caddr    caddr)
    (define/global/1 'cdaar    cdaar)
    (define/global/1 'cdadr    cdadr)
    (define/global/1 'cddar    cddar)
    (define/global/1 'cdddr    cdddr)
    (define/global/1 'caaaar   caaaar)
    (define/global/1 'caaadr   caaadr)
    (define/global/1 'caadar   caadar)
    (define/global/1 'caaddr   caaddr)
    (define/global/1 'cadaar   cadaar)
    (define/global/1 'cadadr   cadadr)
    (define/global/1 'caddar   caddar)
    (define/global/1 'cadddr   cadddr)
    (define/global/1 'cdaaar   cdaaar)
    (define/global/1 'cdaadr   cdaadr)
    (define/global/1 'cdadar   cdadar)
    (define/global/1 'cdaddr   cdaddr)
    (define/global/1 'cddaar   cddaar)
    (define/global/1 'cddadr   cddadr)
    (define/global/1 'cdddar   cdddar)
    (define/global/1 'cddddr   cddddr)
    (define/global/2 'list-ref list-ref)
    (define/global/2 'memq     memq)
    (define/global/2 'memv     memv)
    (define/global/2 'member   member)
    (define/global/2 'assq     assq)
    (define/global/2 'assv     assv)
    (define/global/2 'assoc    assoc)
    (define/global/2 'filter   filter)
    
    ; Logical negation.
    (define/global/1 'not not)
    
    ; Numerics.
    (define/global/N '<           <)
    (define/global/N '>           >)
    (define/global/N '<=          <=)
    (define/global/N '>=          >=)
    (define/global/2 '=           =)
    (define/global/1 'zero?       zero?)
    (define/global/1 'positive?   positive?)
    (define/global/1 'negative?   negative?)
    (define/global/1 'odd?        odd?)
    (define/global/1 'even?       even?)
    (define/global/1 'add1        add1)
    (define/global/1 'sub1        sub1)
    (define/global/1 '1-          sub1) ; Handy alias.
    (define/global/1 '1+          add1) ; Handy alias.
    (define/global/N '+           +)
    (define/global/N '-           -)
    (define/global/N '*           *)
    (define/global/N '/           /)
    (define/global/2 'quotient    quotient)
    (define/global/2 'remainder   remainder)
    (define/global/2 'modulo      modulo)
    (define/global/N 'max         max)
    (define/global/N 'min         min)
    (define/global/N 'gcd         gcd)
    (define/global/N 'lcm         lcm)
    (define/global/1 'numerator   numerator)
    (define/global/1 'denominator denominator)
    (define/global/1 'floor       floor)
    (define/global/1 'ceiling     ceiling)
    (define/global/1 'truncate    truncate)
    (define/global/1 'round       round)
    (define/global/2 'rationalize rationalize)   
    (define/global/1 'exp         exp)
    (define/global/1 'log         log)
    (define/global/1 'sin         sin)
    (define/global/1 'cos         cos)
    (define/global/1 'tan         tan)
    (define/global/1 'asin        asin)
    (define/global/1 'acos        acos)
    (define/global/1 'atan        atan)
    (define/global/1 'sqrt        sqrt)
    (define/global/1 'expt        expt)   
    
    ; Characters.
    (define/global/1 'char?            char?)
    (define/global/2 'char=?           char=?)
    (define/global/2 'char<?           char<?)
    (define/global/2 'char>?           char>?)
    (define/global/2 'char<=?          char<=?)
    (define/global/2 'char>=?          char>=?)
    (define/global/2 'char-ci=?        char-ci=?)
    (define/global/2 'char-ci<?        char-ci<?)
    (define/global/2 'char-ci>?        char-ci>?)
    (define/global/2 'char-ci<=?       char-ci<=?)
    (define/global/2 'char-ci>=?       char-ci>=?)
    (define/global/1 'char-alphabetic? char-alphabetic?)
    (define/global/1 'char-numeric?    char-numeric?)
    (define/global/1 'char-whitespace? char-whitespace?)
    (define/global/1 'char-lower-case? char-lower-case?)
    (define/global/1 'char->integer    char->integer)
    (define/global/1 'integer->char    integer->char)
    (define/global/1 'char-upcase      char-upcase)
    (define/global/1 'char-downcase    char-downcase)
    
    ; Strings.
    (define/global/1 'string?       string?)
    (define/global/N 'make-string   make-string)
    (define/global/N 'string        string)
    (define/global/1 'string-length string-length)
    (define/global/2 'string-ref    string-ref)
    (define/global/2 'string=?      string=?)
    (define/global/2 'string<?      string<?)
    (define/global/2 'string>?      string>?)
    (define/global/2 'string<=?     string<=?)
    (define/global/2 'string>=?     string>=?)
    (define/global/2 'string-ci=?   string-ci=?)
    (define/global/2 'string-ci<?   string-ci<?)
    (define/global/2 'string-ci>?   string-ci>?)
    (define/global/2 'string-ci<=?  string-ci<=?)
    (define/global/2 'string-ci>=?  string-ci>=?)
    (define/global/3 'substring     substring)
    (define/global/N 'string-append string-append)
    
    ; Symbols.
    (define/global/1 'symbol->string symbol->string)
    (define/global/1 'string->symbol string->symbol)
    
    ; Persistent functional vectors.
    (define/global/2     'list/vector       list/vector)
    (define/combinator/2 'vector/build      vector/build)
    (define/combinator/3 'vector/fold/left  vector/fold/left)
    (define/combinator/3 'vector/fold/right vector/fold/right)
    (define/global/1     'vector/persist?   vector/persist?)
    (define/global/1     'vector/length     vector/length)
    (define/global/1     'vector/list       vector/list)
    (cons                'vector/null       vector/null)  ; Pre-defined constant.
    (define/global/1     'vector/null?      vector/null?)
    (define/global/2     'vector/cons       vector/cons)
    (define/global/1     'vector/cdr        vector/cdr)
    (define/combinator/2 'vector/filter     vector/filter)
    (define/combinator/2 'vector/map        vector/map)
    (define/global/2     'vector/ref        vector/ref)
    (define/global/N     'vector/subvector  vector/subvector)
    (define/global/3     'vector/update     vector/update)

    ; Persistent functional hash tables.
    ; Type test for hash tables.
    (define/global/1 'hash/persist?      hash/persist?)
    ; Empty hash tables.
    (cons            'hash/eq/null       hash/eq/null)    ; Pre-defined constant.
    (cons            'hash/eqv/null      hash/eqv/null)   ; Pre-defined constant.
    (cons            'hash/equal/null    hash/equal/null) ; Pre-defined constant.
    ; Hash table constructors.
    (define/global/N    'hash/new        hash/new)
    (define/global/2    'list/hash       list/hash)
    (define/global/2    'pairs/hash      pairs/hash)
    (define/global/3    'hash/cons       hash/cons)
    (define/global/2    'hash/remove     hash/remove)
    (define/global/2    'hash/merge      hash/merge)
    ; Hash table deconstructors.
    (define/global/1     'hash/list      hash/list)
    (define/global/1     'hash/pairs     hash/pairs)
    (define/global/1     'hash/keys      hash/keys) 
    ; Hash table lookup.
    (define/global/3     'hash/ref       hash/ref)
    (define/global/1     'hash/car       hash/car)
    (define/global/1     'hash/cdr       hash/cdr)
    ; Cardinality and membership.
    (define/global/1     'hash/length    hash/length)
    (define/global/1     'hash/empty?    hash/empty?)
    (define/global/2     'hash/contains? hash/contains?)
    ; Hash table combinators.
    (define/combinator/3 'hash/fold      hash/fold)
    (define/combinator/2 'hash/map       hash/map)
    (define/combinator/2 'hash/filter    hash/filter)
    (define/combinator/2 'hash/partition hash/partition)

    ; Persistent functional unordered sets.
    (define/global/1     'set/persist?     set/persist?)
    (cons                'set/eq/null      set/eq/null)
    (cons                'set/eqv/null     set/eqv/null)
    (cons                'set/equal/null   set/equal/null)
    ; Set constructors.
    (define/global/N     'set/new          set/new)
    (define/global/2     'list/set         list/set)
    (define/global/2     'set/cons         set/cons)
    (define/global/2     'set/remove       set/remove)
    (define/global/2     'set/union        set/union)
    (define/global/2     'set/intersection set/intersection)
    (define/global/2     'set/difference   set/difference)
    ; Set deconstructors.
    (define/global/1     'set/list         set/list)
    ; Set lookup.
    (define/global/2     'set/contains?    set/contains?)
    (define/global/1     'set/car          set/car)
    (define/global/1     'set/cdr          set/cdr)
    ; Set cardinality and membership.
    (define/global/1     'set/length       set/length)
    (define/global/1     'set/empty?       set/empty?)
    (define/global/2     'set/subset?      set/subset?)
    ; Set combinators.
    (define/combinator/3 'set/fold         set/fold)
    (define/combinator/2 'set/map          set/map)
    (define/combinator/2 'set/filter       set/filter)
    (define/combinator/2 'set/partition    set/partition)

    ; Tuples.
    (define/global/1     'tuple?           tuple?)
    (define/global/1     'tuple/length     tuple/length)
    (define/global/1     'tuple/list       tuple/list)
    (define/global/2     'tuple/ref        tuple/ref)
    ; Tuple constructors.
    (define/global/N     'tuple            tuple)
    (define/global/1     'list/tuple       list/tuple)
    (define/global/N     'tuple/append     tuple/append)
    ; Tuple slicing.
    (define/global/N     'tuple/copy       tuple/copy)
    (define/global/2     'tuple/drop/left  tuple/drop/left)
    (define/global/2     'tuple/drop/right tuple/drop/right)
    (define/global/2     'tuple/take/left  tuple/take/left)
    (define/global/2     'tuple/take/right tuple/take/right)
    ; Tuple combinators.
    (define/combinator/2 'tuple/build      tuple/build)
    (define/combinator/2 'tuple/filter     tuple/filter)
    (define/combinator/2 'tuple/map        tuple/map)
    (define/combinator/2 'tuple/partition  tuple/partition)

    ; Binding environments
    (cons 'environ/null environ/null)
    (cons 'environ/capture
          ; This one has to be hand crafted.
          (let ((descriptor (vector 'reference/global 'environ/capture)))
            (lambda (k e global)
              (if k (k global) descriptor))))
    (define/global/2 'environ/merge environ/merge)

    ; Higher order functions.
    (cons            'apply     motile/apply)
    (cons            'map       motile/map)
    (cons            'for-each  motile/for-each)

    ; Control
    (cons 'call/cc call/cc)
    (cons 'call-with-current-continuation call/cc) ; Synonym.

    ; Box
    (define/global/1 'box      box)
    (define/global/1 'box?     box?)
    (define/global/1 'unbox    unbox)
    (define/global/2 'box!     set-box!)
    (define/global/2 'set-box! set-box!) ; For compatibility with Racket.

    ; Generic list sort.
    (cons 'sort motile/sort)
    )))

(define ENVIRON/TEST
  (pairs/environ
   BASELINE
   (list
    ; Mutable vectors for some regression tests.
    (define/global/1 'vector?       vector?)
    (define/global/N 'make-vector   make-vector)
    (define/global/N 'vector        vector)
    (define/global/1 'vector-length vector-length)
    (define/global/2 'vector-ref    vector-ref)
    (define/global/3 'vector-set!   vector-set!)

    ; For simple test output.
    (define/global/1 'display        display)
    (define/global/N 'format         format)
    (define/global/0 'newline        newline)
    (define/global/1 'pretty-display pretty-display))))

;(define/global/1 'make-rectangular               make-rectangular)
;;(def-proc 'make-polar                     make-polar)
;;(def-proc 'real-part                      real-part)
;;(def-proc 'imag-part                      imag-part)
;;(def-proc 'magnitude                      magnitude)
;;(def-proc 'angle                          angle)
;(def-proc 'call-with-input-file           call-with-input-file)
;(def-proc 'call-with-output-file          call-with-output-file)
;(def-proc 'input-port?                    input-port?)
;(def-proc 'output-port?                   output-port?)
;(def-proc 'current-input-port             current-input-port)
;(def-proc 'current-output-port            current-output-port)
;(def-proc 'open-input-file                open-input-file)
;(def-proc 'open-output-file               open-output-file)
;(def-proc 'close-input-port               close-input-port)
;(def-proc 'close-output-port              close-output-port)
;(def-proc 'eof-object?                    eof-object?)
;(def-proc 'read                           read)
;(def-proc 'read-char                      read-char)
;(def-proc 'peek-char                      peek-char)
;(def-proc 'write                          write)
;(def-proc 'display                        display)
;(def-proc 'newline                        newline)
;(def-proc 'write-char                     write-char)
