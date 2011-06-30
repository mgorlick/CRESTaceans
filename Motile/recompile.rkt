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

;; The recompiler which accepts a Mischief textual closure representation as input and returns
;; a regenerated closure as output.

(require
 racket/vector
 "dissect.ss"
 (only-in
  "compile.ss"
  and/generate
  closure/inner/generate
  closure/generate
  closure/rest/inner/generate
  closure/rest/generate
  combination/generate
  constant/generate
  environ/cons/generate
  environ/reflect/generate
  environ/remove/generate
  environ/value/generate
  if/generate
  lambda/inner/generate
  lambda/generate
  lambda/rest/inner/generate
  lambda/rest/generate
  motile/start
  or/generate
  reference/global/variable/generate
  sequence/generate
  setter/generate
  should-be ; For testing only. To be removed when we upgrade to rackunit.
  unless/generate
  when/generate
  )
 )
          

(provide
 code/closure/inner/bindings
 code/closure/inner/bindings/span
 code/closure/inner?
 code/closure/rest/inner?
 code/lambda/inner?
 code/lambda/rest/inner?
 mischief/recompile/unit
 motile/recompile/unit)

(define (recompile/error where e)
  (error where "ill-formed: ~a" e))

(define-syntax-rule (code/constant/generate? e)
  (eq? 'constant/generate (vector-ref e 0)))
(define-syntax-rule (constant/generate/value e)
  (vector-ref e 1))
(define (code/constant/ok? e)
  (if (and
       (= (vector-length e) 2)) ; Need to add cycle checking here.
      #t
      (recompile/error 'recompile/code/constant e)))

(define-syntax-rule (code/lambda? e)
  (eq? 'lambda (vector-ref e 0)))
(define-syntax-rule (code/lambda/arity e)
  (vector-ref e 1))
(define-syntax-rule (code/lambda/body e)
  (vector-ref e 2))
(define (code/lambda/ok? e)
  (if (and
       (= (vector-length e) 3)
       (integer? (code/lambda/arity e))
       (>= (code/lambda/arity e) 0))
      #t
      (recompile/error 'recompile/code/lambda e)))

(define-syntax-rule (code/lambda/inner? e)
  (eq? 'lambda/inner (vector-ref e 0)))
(define-syntax-rule (code/lambda/inner/arity e)
  (vector-ref e 1))
(define-syntax-rule (code/lambda/inner/body e)
  (vector-ref e 2))
(define (code/lambda/inner/ok? e)
  (if (and
       (= (vector-length e) 3)
       (integer? (code/lambda/inner/arity e))
       (>= (code/lambda/inner/arity e) 0))
      #t
      (recompile/error 'recompile/code/lambda/inner e)))

(define-syntax-rule (code/lambda/rest? e)
  (eq? 'lambda/rest (vector-ref e 0)))
(define-syntax-rule (code/lambda/rest/arity e)
  (vector-ref e 1))
(define-syntax-rule (code/lambda/rest/body e)
  (vector-ref e 2))
(define (code/lambda/rest/ok? e)
  (if (and
       (= (vector-length e) 3)
       (integer? (code/lambda/rest/arity e))
       (> (code/lambda/rest/arity e) 0))
       #t
      (recompile/error 'recompile/code/lambda/rest e)))

(define-syntax-rule (code/lambda/rest/inner? e)
  (eq? 'lambda/rest/inner (vector-ref e 0)))
(define (code/lambda/rest/inner/ok? e)
  (if (and
       (= (vector-length e) 3)
       (integer? (code/lambda/rest/arity e))
       (> (code/lambda/rest/arity e) 0))
       #t
      (recompile/error 'recompile/code/lambda/rest/inner e)))

(define-syntax-rule (code/reference/parameter? e)
  (eq? 'reference/parameter (vector-ref e 0)))
(define-syntax-rule (code/reference/parameter/offset e)
  (vector-ref e 1))
(define (code/reference/parameter/ok? e)
  (if (and
       (= (vector-length e) 2)
       (integer? (code/reference/parameter/offset e))
       (> (code/reference/parameter/offset e) 0))
      #t
      (recompile/error 'recompile/reference/parameter e)))

(define-syntax-rule (code/reference/closed? e)
  (eq? 'reference/closed (vector-ref e 0)))
(define-syntax-rule (code/reference/closed/offset e)
  (vector-ref e 1))
(define (code/reference/closed/ok? e)
  (if (and
       (= (vector-length e) 2)
       (integer? (code/reference/closed/offset e))
       (> (code/reference/closed/offset e) 0))
      #t
      (recompile/error 'recompile/reference/closed e)))

(define-syntax-rule (code/reference/global? e)
  (eq? 'reference/global (vector-ref e 0)))
(define-syntax-rule (code/reference/global/symbol e)
  (vector-ref e 1))
(define (code/reference/global/ok? e)
  (if (and
       (vector? e)
       (= (vector-length e) 2)
       (symbol? (code/reference/global/symbol e)))
      #t
      (recompile/error 'recompile/reference/global e)))

(define-syntax-rule (code/combination? e)
  (eq? 'combination (vector-ref e 0)))
(define-syntax-rule (code/combination/arity e)
  (vector-ref e 1))
(define-syntax-rule (code/combination/operator e)
  (vector-ref e 2))
(define-syntax-rule (code/combination/arguments e)
  (vector-ref e 3))
(define (code/combination/ok? e)
  (if (and
        (= (vector-length e) 4)
       (integer? (code/combination/arity e))
       (>= (code/combination/arity e) 0)
       (= (length (code/combination/arguments e)) (code/combination/arity e)))
      #t
      (recompile/error 'recompile/combination e)))
  
(define-syntax-rule (code/closure? e)
  (eq? 'closure (vector-ref e 0)))
(define-syntax-rule (code/closure/arity e)
  (vector-ref e 1))
(define-syntax-rule (code/closure/addresses/span e)
  (vector-ref e 2))
(define-syntax-rule (code/closure/addresses e)
  (vector-ref e 3))
(define-syntax-rule (code/closure/body e)
  (vector-ref e 4))
(define-syntax-rule (code/closure/address/ok? a)
  (and
   (pair? a)
   (integer? (car a))
   (integer? (cdr a))
   (>= (car a) 0)   ; frame
   (>  (cdr a) 0))) ; offset

(define (code/closure/addresses/ok? addresses)
  (cond
    ((null? addresses) #t)
    ((code/closure/address/ok? (car addresses))
     (code/closure/addresses/ok? (cdr addresses)))
    (else #f)))
      
(define (code/closure/ok? e)
  (if (and
       (= (vector-length e) 5)
       (integer? (code/closure/arity e))
       (>= (code/closure/arity e) 0)
       (integer? (code/closure/addresses/span e))
       (> (code/closure/addresses/span e) 0)
       (list? (code/closure/addresses e))
       (= (code/closure/addresses/span e) (length (code/closure/addresses e)))
       (code/closure/addresses/ok? (code/closure/addresses e)))
      #t
      (recompile/error 'recompile/code/closure e)))

(define-syntax-rule (code/closure/rest? e)
  (eq? 'closure/rest (vector-ref e 0)))
(define (code/closure/rest/ok? e)
  (if (and
       (= (vector-length e) 5)
       (integer? (code/closure/arity e))
       (positive? (code/closure/arity e))
       (integer? (code/closure/addresses/span e))
       (positive? (code/closure/addresses/span e))
       (list? (code/closure/addresses e))
       (= (length (code/closure/addresses e)) (code/closure/addresses/span e))
       (code/closure/addresses/ok? (code/closure/addresses e)))
      #t
      (recompile/error 'recompile/code/closure/rest e)))

;; A code/closure/inner is a vector #('closure/inner m n bindings body) where:
;; m - total number of formal lambda parameters
;; n - total number of closed variables x_0, ..., x_{n-1} in lambda body
;; bindings - vector of length n where v[i] is the value of the binding for closed variable x{i]
;; body - the body of the lambda expression. 
(define-syntax-rule (code/closure/inner? e)
  (eq? 'closure/inner (vector-ref e 0)))
(define-syntax-rule (code/closure/inner/arity e)
  (vector-ref e 1))
(define-syntax-rule (code/closure/inner/bindings/span e)
  (vector-ref e 2))
(define-syntax-rule (code/closure/inner/bindings e)
  (vector-ref e 3))
(define-syntax-rule (code/closure/inner/body e)
  (vector-ref e 4))
(define (code/closure/inner/ok? e)
  (if (and
       (= (vector-length e) 5)
       (integer? (code/closure/inner/arity e))
       (>= (code/closure/inner/arity e) 0)
       (integer? (code/closure/inner/bindings/span e))
       (positive?  (code/closure/inner/bindings/span e))
       (vector? (code/closure/inner/bindings e))
       (= (code/closure/inner/bindings/span e) (vector-length (code/closure/inner/bindings e))))
      #t
      (recompile/error 'recompile/code/closure/inner e)))

(define-syntax-rule (code/closure/rest/inner? e)
  (eq? 'closure/rest/inner (vector-ref e 0)))
(define (code/closure/rest/inner/ok? e)
  (if (and
       (= (vector-length e) 5)
       (integer? (code/closure/inner/arity e))
       (>= (code/closure/arity e) 0)
       (integer? (code/closure/inner/bindings/span e))
       (positive?  (code/closure/inner/bindings/span e))
       (vector? (code/closure/inner/bindings e))
       (= (code/closure/inner/bindings/span e) (vector-length (code/closure/inner/bindings e))))
      #t
      (recompile/error 'recompile/code/closure/rest/inner e)))

(define-syntax-rule (code/sequence? e)
  (eq? 'sequence (vector-ref e 0)))
(define-syntax-rule (code/sequence/length e)
  (vector-ref e 1))
(define-syntax-rule (code/sequence/elements e)
  (vector-ref e 2))
(define (code/sequence/ok? e)
  (if (and
       (= (vector-length e) 3)
       (integer? (code/sequence/length e))
       (> (code/sequence/length e) 0)
       (list? (code/sequence/elements e))
       (= (code/sequence/length e) (length (code/sequence/elements e))))
      #t
      (recompile/error 'recompile/code/sequence e)))

(define-syntax-rule (code/setter? e)
  (eq? 'setter (vector-ref e 0)))
(define-syntax-rule (code/setter/span e)
  (vector-ref e 1))
(define (code/setter/ok? e)
  (if (and
       (= (vector-length e) 2)
       (integer? (code/setter/span e))
       (positive? (code/setter/span e)))
      #t
      (recompile/error 'recompile/code/setter e)))

;; OBSOLETE form of setter.
;(define-syntax-rule (code/setter/address e)
;  (vector-ref e 1))
;(define-syntax-rule (code/setter/value e)
;  (vector-ref e 2))
;(define-syntax-rule (code/setter/address/ok? a)
;  (and
;   (pair? a)
;   (integer? (car a))
;   (integer? (cdr a))
;   (> (car a) 0)   ; frame
;   (> (cdr a) 0))) ; offset
;(define (code/setter/ok? e)
;  (if (and
;       (= (vector-length e) 3)
;       (code/setter/address/ok? (code/setter/address e)))
;      #t
;      (recompile/error 'recompile/code/setter e)))

(define-syntax-rule (code/if? e)
  (eq? 'if (vector-ref e 0)))
(define-syntax-rule (code/if/test e)
  (vector-ref e 1))
(define-syntax-rule (code/if/then e)
  (vector-ref e 2))
(define-syntax-rule (code/if/else e)
  (vector-ref e 3))
(define (code/if/ok? e)
  (if (= (vector-length e) 4)
      #t
      (recompile/error 'recompile/code/if e)))

(define-syntax-rule (code/and? e)
  (eq? 'and (vector-ref e 0)))
(define-syntax-rule (code/and/head e)
  (vector-ref e 1))
(define-syntax-rule (code/and/tail e)
  (vector-ref e 2))
(define (code/and/ok? e)
  (if (= (vector-length e) 3)
      #t
      (recompile/error 'recompile/code/and e)))

(define-syntax-rule (code/or? e)
  (eq? 'or (vector-ref e 0)))
(define-syntax-rule (code/or/head e)
  (vector-ref e 1))
(define-syntax-rule (code/or/tail e)
  (vector-ref e 2))
(define (code/or/ok? e)
  (if (= (vector-length e) 3)
      #t
      (recompile/error 'recompile/code/or e)))

(define-syntax-rule (code/when? e)
  (eq? 'when (vector-ref e 0)))
(define-syntax-rule (code/when/test e)
  (vector-ref e 1))
(define-syntax-rule (code/when/thens e)
  (vector-ref e 2))
(define (code/when/ok? e)
  (if (= (vector-length e) 3)
      #t
      (recompile/error 'recompile/code/when e)))

(define-syntax-rule (code/unless? e)
  (eq? 'unless (vector-ref e 0)))
(define-syntax-rule (code/unless/test e)
  (vector-ref e 1))
(define-syntax-rule (code/unless/thens e)
  (vector-ref e 2))
(define (code/unless/ok? e)
  (if (= (vector-length e) 3)
      #t
      (recompile/error 'recompile/code/unless e)))

;; Reference to a formal parameter within the body of a lambda expression.
(define (code/reference/parameter/recompile offset)
  (lambda (k e _)
    (if k
        (k (vector-ref e offset))
        (vector 'reference/parameter offset))))
;  (lambda (rtk rte) 
;    (if rtk
;        (rtk (vector-ref rte offset))
;        (vector 'reference/parameter offset))))

;; Reference to a closed variable within the body of a lambda expression.
;; Since both parameter values and closed variable bindings are contained with the same stack frame
;; the code to reference a closed variable is identical to that for referencing a parameter.
(define (code/reference/closed/recompile offset)
  (lambda (k e _)
    (if k
        (k (vector-ref e offset))
        (vector 'reference/closed offset))))
;  (lambda (rtk rte)
;    (if rtk
;        (rtk (vector-ref rte offset))
;        (vector 'reference/closed offset))))


;    ; (environ/reflect E e_1 ... e_n)
;    ((environ/reflect? e)
;     (environ/reflect/compile e lexical))

;; Recompilation for binding environment special forms.
(define-syntax-rule (code/environ/cons? e)
  (eq? 'environ/cons (vector-ref e 0)))
(define-syntax-rule (code/environ/cons/identifiers/length e)
  (vector-ref e 1))
(define-syntax-rule (code/environ/cons/environ e)
  (vector-ref e 2))
(define-syntax-rule (code/environ/cons/identifiers e) ; A vector of identifiers NOT a list!
  (vector-ref e 3))
(define-syntax-rule (code/environ/cons/values e) ; A vector of Motile codes, one per identifier.
  (vector-ref e 4))
(define (code/environ/cons/ok? e)
  (if (and
       (= (vector-length e) 5)

       ; The lengths of all of the pieces must match up.
       (let ((n  (code/environ/cons/identifiers/length e)))
         (and
          (integer? n)
          (positive? n)
          (= n (vector-length (code/environ/cons/identifiers e)))
          (= n (vector-length (code/environ/cons/values e)))))

       ; Every element of the values vector must be a valid parameter or closed variable reference.
       (and/vector
        (lambda (v)
          (cond
            ((code/reference/parameter? v) (code/reference/parameter/ok? v))
            ((code/reference/closed? v)    (code/reference/closed/ok?    v))
            (else #f)))
        (code/environ/cons/values e))

       ; Every identifier must be a symbol.
       (and/vector symbol? (code/environ/cons/identifiers e)))

      #t
      
      (recompile/error 'recompile/code/environ/cons e)))
       
(define (and/vector predicate v)
  (= (vector-length v) (vector-count predicate v)))

(define-syntax-rule (code/environ/value? e)
  (eq? 'environ/value (vector-ref e 0)))
(define-syntax-rule (code/environ/value/environ e)
  (vector-ref e 1))
(define-syntax-rule (code/environ/value/symbol e)
  (vector-ref e 2))
(define-syntax-rule (code/environ/value/failure e)
  (vector-ref e 3))
(define (code/environ/value/ok? e)
  (if (and (= (vector-length e) 4) (symbol? (code/environ/value/symbol e)))
      #t
      (recompile/error 'recompile/code/environ/value e)))

(define-syntax-rule (code/environ/remove? e)
  (eq? 'environ/remove (vector-ref e 0)))
(define-syntax-rule (code/environ/remove/symbols/length e)
  (vector-ref e 1))
(define-syntax-rule (code/environ/remove/environ e)
  (vector-ref e 2))
(define-syntax-rule (code/environ/remove/symbols e)
  (vector-ref e 3))
(define (code/environ/remove/ok? e)
  (if (and
       (= (vector-length e) 4)
       (let ((n (code/environ/remove/symbols/length e)))
         (and
          (integer? n)
          (positive? n)
          (= n (vector-length (code/environ/remove/symbols e)))))
       (and/vector symbol? (code/environ/remove/symbols e)))
      #t
      (recompile/error 'recompile/code/environ/remove e)))

(define-syntax-rule (code/environ/reflect? e)
  (eq? 'environ/reflect (vector-ref e 0)))
(define-syntax-rule (code/environ/reflect/environ e)
  (vector-ref e 1))
(define-syntax-rule (code/environ/reflect/body e)
  (vector-ref e 2))
(define (code/environ/reflect/ok? e)
  (if (= (vector-length e) 3)
      #t
      (recompile/error 'recompile/code/environ/reflect e)))

(define (code/ok? e)
  (if (vector? e)
      #t
      (recompile/error 'recompile/code e)))

(define (mischief/recompile/unit e)
  (code/ok? e)
  (cond
    ((code/constant/generate? e)
     (constant/generate (constant/generate/value e)))
    
    ((code/lambda? e)
     (code/lambda/ok? e)
     (lambda/generate (code/lambda/arity e) (mischief/recompile/unit (code/lambda/body e))))
    
    ((code/lambda/inner? e)
     (code/lambda/inner/ok? e)
     (lambda/inner/generate
      (code/lambda/inner/arity e)
      (mischief/recompile/unit (code/lambda/inner/body e))))

    ((code/lambda/rest? e)
     (code/lambda/rest/ok? e)
     (lambda/rest/generate (code/lambda/rest/arity e) (mischief/recompile/unit (code/lambda/rest/body e))))

    ((code/lambda/rest/inner? e)
     (code/lambda/rest/inner/ok? e)
     (lambda/rest/inner/generate (code/lambda/rest/arity e) (mischief/recompile/unit (code/lambda/rest/body e))))

    ((code/closure? e)
     (code/closure/ok? e)
     (closure/generate
      (code/closure/arity e)
      (code/closure/addresses/span e)
      (code/closure/addresses e)
      (mischief/recompile/unit (code/closure/body e))))

    ((code/closure/inner? e)
     (code/closure/inner/ok? e)
     (closure/inner/generate
      (code/closure/inner/arity e)
      (mischief/recompile/unit (code/closure/inner/body e))))

    ((code/closure/rest? e)
     (code/closure/rest/ok? e)
     (closure/rest/generate
      (code/closure/arity e)
      (code/closure/addresses e)
      (mischief/recompile/unit (code/closure/body e))))

    ((code/closure/rest/inner? e)
     (code/closure/rest/inner/ok? e)
     (closure/rest/inner/generate
      (code/closure/inner/arity e)
      (mischief/recompile/unit (code/closure/inner/body e))))

    ((code/reference/parameter? e)
     (code/reference/parameter/ok? e)
     (code/reference/parameter/recompile (code/reference/parameter/offset e)))

    ((code/reference/closed? e)
     (code/reference/closed/ok? e)
     (code/reference/closed/recompile (code/reference/closed/offset e)))

    ((code/reference/global? e)
     (code/reference/global/ok? e)
     (reference/global/variable/generate (code/reference/global/symbol e)))

    ((code/combination? e)
     (code/combination/ok? e)
     (combination/generate
       (mischief/recompile/unit (code/combination/operator e))
       (map (lambda (a) (mischief/recompile/unit a)) (code/combination/arguments e))))

    ((code/sequence? e)
     (code/sequence/ok? e)
     (sequence/generate
      (code/sequence/length e)
      (map (lambda (e) (mischief/recompile/unit e)) (code/sequence/elements e))))
    
    ((code/setter? e)
     (code/setter/ok? e)
     (setter/generate (code/setter/span e)))
;      (code/setter/address e)
;      (mischief/recompile/unit (code/setter/value e))))
      
    ((code/if? e)
     (code/if/ok? e)
     (if/generate
      (mischief/recompile/unit (code/if/test e))
      (mischief/recompile/unit (code/if/then e))
      (mischief/recompile/unit (code/if/else e))))

    ((code/and? e)
     (code/and/ok? e)
     (and/generate
      (mischief/recompile/unit (code/and/head e))
      (mischief/recompile/unit (code/and/tail e))))
    
    ((code/or? e)
     (code/or/ok? e)
     (or/generate
      (mischief/recompile/unit (code/or/head e))
      (mischief/recompile/unit (code/or/tail e))))
    
    ((code/when? e)
     (code/when/ok? e)
     (when/generate
      (mischief/recompile/unit (code/when/test e))
      (mischief/recompile/unit (code/when/thens e))))

    ((code/unless? e)
     (code/unless/ok? e)
     (unless/generate
      (mischief/recompile/unit (code/unless/test e))
      (mischief/recompile/unit (code/unless/thens e))))

    ((code/environ/cons? e)
     (code/environ/cons/ok? e)
     (environ/cons/generate
      (motile/recompile/unit (code/environ/cons/environ e))
      (code/environ/cons/identifiers e)
      (vector-map motile/recompile/unit (code/environ/cons/values e))))
    
    ((code/environ/value? e)
     (code/environ/value/ok? e)
     (environ/value/generate
      (motile/recompile/unit (code/environ/value/environ e))
      (code/environ/value/symbol e)
      (motile/recompile/unit (code/environ/value/failure e))))

    ((code/environ/remove? e)
     (code/environ/remove/ok? e)
     (environ/remove/generate
      (motile/recompile/unit (code/environ/remove/environ e))
      (code/environ/remove/symbols e)))

    ((code/environ/reflect? e)
     (code/environ/reflect/ok? e)
     (environ/reflect/generate
      (motile/recompile/unit (code/environ/reflect/environ e))
      (motile/recompile/unit (code/environ/reflect/body e))))
    
    (else
     (recompile/error 'mischief/recompile/unit e))))

(define motile/recompile/unit mischief/recompile/unit)


;(define (test/recompile/lambda)
;  ; Zero parameter lambda expression.
;  (define (recompile/lambda/1)
;    (let* ((code #(lambda 0 #(constant/generate 13))) ; (lambda () 13)
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/lambda/1 13 ((start x) rtk/RETURN))))
;  
;  ; Application of a one parameter lambda expression.
;  (define (recompile/lambda/2)
;    (let* ((code #(combination 1 #(lambda 1 #(reference/parameter 1)) (#(constant/generate 13)))) ; ((lambda (x) x) 13)
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/lambda/2 13 (start x))))
;  
;  ; Application of a two parameter lambda expression.
;  (define (recompile/lambda/3)
;    (let* ((code
;            ; ((lambda (x y) (* x y)) -7 3)
;            #(combination
;              2
;              #(lambda 2 #(combination 2 #(reference/global *) (#(reference/parameter 1) #(reference/parameter 2))))
;              (#(constant/generate -7) #(constant/generate 3))))
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/lambda/3 -21 (start x))))  
;
;  (recompile/lambda/1)
;  (recompile/lambda/2)
;  (recompile/lambda/3))
;
;(define (test/recompile/lambda/rest)
;  (define (recompile/lambda/rest/1)
;    (let* ((code ; ((lambda rest rest) 1 2 3 4)
;            #(combination
;              4
;              #(lambda/rest 1 #(reference/parameter 1))
;              (#(constant/generate 1) #(constant/generate 2) #(constant/generate 3) #(constant/generate 4))))
;           (x (mischief/recompile/unit code)))
;      (should-be 'lambda/rest/1 '(1 2 3 4) (start x))))
;  
;  (define (recompile/lambda/rest/2)
;    (let* ((code ; ((lambda (a b . rest) (list a b rest)) 1 2 3 4)
;            #(combination
;              4
;              #(lambda/rest
;                3
;                #(combination
;                  3
;                  #(reference/global list)
;                  (#(reference/parameter 1) #(reference/parameter 2) #(reference/parameter 3))))
;              (#(constant/generate 1) #(constant/generate 2) #(constant/generate 3) #(constant/generate 4))))
;           (x (mischief/recompile/unit code)))
;      (should-be 'lambda/rest/2 '(1 2 (3 4)) (start x))))
;
;  (recompile/lambda/rest/1)
;  (recompile/lambda/rest/2))
;
;(define (test/recompile/combination)
;  ; A zero argument combination.
;  (define (recompile/combination/1)
;    (let* ((code #(combination 0 #(lambda 0 #(constant/generate 13)) ())) ; ((lambda () 13))
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/combination/1 13 (start x))))
;      
;  ; A one argument combination.
;  (define (recompile/combination/2)
;    (let* ((code #(combination 1 #(reference/global list) (#(constant/generate 13)))) ; (list 13)
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/combination/2 '(13) (start x))))
;           
;  ; A two argument combination.
;  (define (recompile/combination/3)
;    (let* ((code #(combination 2 #(reference/global +) (#(constant/generate 3) #(constant/generate 17)))) ; (+ 3 17)
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/combination/3 20 (start x)))) 
;
;  ; A three argument combination.
;  (define (recompile/combination/4)
;    (let* ((code
;            ; (max 7 13 9)
;            #(combination 3 #(reference/global max) (#(constant/generate 7) #(constant/generate 13) #(constant/generate 9))))
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/combination/4 13 (start x))))
;
;  ; An N > 3 argument combination.
;  (define (recompile/combination/5)
;    (let* ((code
;            ; (list 'foo 33 92 "silly" (cons -1 -12))
;            #(combination
;              5
;              #(reference/global list)
;              (#(constant/generate foo)
;               #(constant/generate 33)
;               #(constant/generate 92)
;               #(constant/generate "silly")
;               #(combination 2 #(reference/global cons) (#(constant/generate -1) #(constant/generate -12))))))
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/combination/5 '(foo 33 92 "silly" (-1 . -12)) (start x))))
;
;  (recompile/combination/1)
;  (recompile/combination/2)
;  (recompile/combination/3)
;  (recompile/combination/4)
;  (recompile/combination/5))
;
;(define (test/recompile/closure)
;  (define (recompile/closure/1)
;    (let* ((code
;            ; ((let ((a 5)) (lambda (b) (+ a b))) 17)
;            #(combination
;              1
;              #(combination
;                1
;                #(lambda
;                     1
;                   #(closure 1 ((0 . 1)) #(combination 2 #(reference/global +) (#(reference/closed 2) #(reference/parameter 1)))))
;                (#(constant/generate 5)))
;              (#(constant/generate 17))))
;           (x (mischief/recompile/unit code)))
;      (should-be 'closure/1 22 (start x))))
;
;
;  (define (recompile/closure/2)
;    (let* ((code
;            ;(let ((h
;            ;       ((lambda (a)
;            ;          ((lambda (b)
;            ;             ((lambda (c)
;            ;                ((lambda (d)
;            ;                   ((lambda (e) (lambda (f) (list a b c d e f))) 5)) 4)) 3)) 2)) 1)))
;            ;  (h 99))
;            #(combination
;              1
;              #(lambda 1 #(combination 1 #(reference/parameter 1) (#(constant/generate 99))))
;              (#(combination
;                 1
;                 #(lambda
;                      1
;                    #(combination
;                      1
;                      #(lambda
;                           1
;                         #(combination
;                           1
;                           #(lambda
;                                1
;                              #(combination
;                                1
;                                #(lambda
;                                     1
;                                   #(combination
;                                     1
;                                     #(lambda
;                                          1
;                                        #(closure
;                                          1
;                                          ((4 . 1) (3 . 1) (2 . 1) (1 . 1) (0 . 1))
;                                          #(combination
;                                            6
;                                            #(reference/global list)
;                                            (#(reference/closed 2)
;                                             #(reference/closed 3)
;                                             #(reference/closed 4)
;                                             #(reference/closed 5)
;                                             #(reference/closed 6)
;                                             #(reference/parameter 1)))))
;                                     (#(constant/generate 5))))
;                                (#(constant/generate 4))))
;                           (#(constant/generate 3))))
;                      (#(constant/generate 2))))
;                 (#(constant/generate 1))))))
;           (x (mischief/recompile/unit code)))
;      (should-be 'closure/2 '(1 2 3 4 5 99) (start x))))
;  
;  (recompile/closure/1)
;  (recompile/closure/2))
;
;(define (test/recompile/closure/rest)
;  (define (recompile/closure/rest/1)
;    (let* ((code ; ((let ((a 5)) (lambda rest (cons a rest))) 17 18 19)
;            #(combination
;              3
;              #(combination
;                1
;                #(lambda
;                     1
;                   #(closure/rest
;                     1
;                     ((0 . 1))
;                     #(combination 2 #(reference/global cons) (#(reference/closed 2) #(reference/parameter 1)))))
;                (#(constant/generate 5)))
;              (#(constant/generate 17) #(constant/generate 18) #(constant/generate 19))))
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/closure/rest/1 '(5 17 18 19) (start x))))
;  
;  (define (recompile/closure/rest/2)
;    (let* ((code
;            (mischief/decompile
;             (mischief/compile
;              '(let ((h
;                      ((lambda (a)
;                         ((lambda (b)
;                            ((lambda (c)
;                               ((lambda (d)
;                                  ((lambda (e) (lambda (f g . rest) (list a b c d e f g rest))) 5)) 4)) 3)) 2)) 1)))
;                 (h 99 100 200 300 400)))))
;           (x (mischief/recompile/unit code)))
;      (should-be 'closure/rest/2 '(1 2 3 4 5 99 100 (200 300 400)) (start x))))
;           
;  (recompile/closure/rest/1)
;  (recompile/closure/rest/2))
;
;
;(define (test/recompile/sequence)
;  (define (recompile/sequence/1)
;    (let* ((code
;            ; ((lambda () (display "1 ") (display "2 ") (display "85\n") 85))
;            #(combination
;              0
;              #(lambda
;                   0
;                 #(sequence
;                    4
;                    (#(combination 1 #(reference/global display) (#(constant/generate "1 ")))
;                     #(combination 1 #(reference/global display) (#(constant/generate "2 ")))
;                     #(combination 1 #(reference/global display) (#(constant/generate "85\n")))
;                     #(constant/generate 85))))
;              ()))
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/sequence/1 85 (start x))))
;                 
;  (recompile/sequence/1))
;
;(define (test/recompile/setter)
;  ; Note: Setters appear only in the code generated for letrec.
;  (define (recompile/setter/1)
;    (let* ((code
;            ; Mutually recursive functions.
;            ;(letrec
;            ;    ((even?
;            ;      (lambda (n)
;            ;        (if (= n 0) #t (odd? (- n 1)))))
;            ;     (odd?
;            ;      (lambda (n)
;            ;        (if (= n 0) #f (even? (- n 1))))))
;            ;  (list
;            ;   (even? 12) (even? 3)
;            ;   (odd? 17)  (odd? 8)))))
;            #(combination
;              2
;              #(lambda
;                   2
;                 #(combination
;                   2
;                   #(closure
;                     2
;                     ((0 . 1) (0 . 2))
;                     #(sequence
;                        3
;                        (#(setter (1 . 1) #(reference/parameter 1))
;                         #(setter (1 . 2) #(reference/parameter 2))
;                         #(combination
;                           0
;                           #(closure
;                             0
;                             ((1 . 1) (1 . 2))
;                             #(combination
;                               4
;                               #(reference/global list)
;                               (#(combination 1 #(reference/closed 1) (#(constant/generate 12)))
;                                #(combination 1 #(reference/closed 1) (#(constant/generate 3)))
;                                #(combination 1 #(reference/closed 2) (#(constant/generate 17)))
;                                #(combination 1 #(reference/closed 2) (#(constant/generate 8))))))
;                           ()))))
;                   (#(closure
;                      1
;                      ((0 . 2))
;                      #(if
;                        #(combination 2 #(reference/global =) (#(reference/parameter 1) #(constant/generate 0)))
;                        #(constant/generate #t)
;                        #(combination
;                          1
;                          #(reference/closed 2)
;                          (#(combination 2 #(reference/global -) (#(reference/parameter 1) #(constant/generate 1)))))))
;                    #(closure
;                      1
;                      ((0 . 1))
;                      #(if
;                        #(combination 2 #(reference/global =) (#(reference/parameter 1) #(constant/generate 0)))
;                        #(constant/generate #f)
;                        #(combination
;                          1
;                          #(reference/closed 2)
;                          (#(combination 2 #(reference/global -) (#(reference/parameter 1) #(constant/generate 1))))))))))
;              (#(constant/generate undefined) #(constant/generate undefined))))
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/setter/1 '(#t #f #t #f) (start x))))
;
;  (recompile/setter/1))
;
;(define (test/recompile/and)
;  (define (recompile/and/1)
;    (let* ((code
;            ;(let ()
;            ;  (define (element n a b)
;            ;    (display (format "element: ~a\n" n))
;            ;    (< a b))
;            ;  (and (element 1 3 5) (element 2 5 3) (element 3 7 8))))) ; Only elements 1 and 2 should execute.
;            #(combination
;              0
;              #(lambda
;                   0
;                 #(combination
;                   1
;                   #(lambda
;                        1
;                      #(combination
;                        1
;                        #(closure
;                          1
;                          ((0 . 1))
;                          #(sequence
;                             2
;                             (#(setter (1 . 1) #(reference/parameter 1))
;                              #(combination
;                                0
;                                #(closure
;                                  0
;                                  ((1 . 1))
;                                  #(and
;                                    #(combination
;                                      3
;                                      #(reference/closed 1)
;                                      (#(constant/generate 1) #(constant/generate 3) #(constant/generate 5)))
;                                    #(and
;                                      #(combination
;                                        3
;                                        #(reference/closed 1)
;                                        (#(constant/generate 2) #(constant/generate 5) #(constant/generate 3)))
;                                      #(combination
;                                        3
;                                        #(reference/closed 1)
;                                        (#(constant/generate 3) #(constant/generate 7) #(constant/generate 8))))))
;                                ()))))
;                        (#(lambda
;                              3
;                            #(sequence
;                               2
;                               (#(combination
;                                  1
;                                  #(reference/global display)
;                                  (#(combination
;                                     2
;                                     #(reference/global format)
;                                     (#(constant/generate "element: ~a\n") #(reference/parameter 1)))))
;                                #(combination 2 #(reference/global <) (#(reference/parameter 2) #(reference/parameter 3)))))))))
;                   (#(constant/generate undefined))))
;              ()))
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/and/1 #f (start x))))
;  
;  (define (recompile/and/2)
;    (let* ((code
;            ;(let ()
;            ;  (define (element n a b)
;            ;    (display (format "element: ~a\n" n))
;            ;    (< a b))
;            ;  (and (element 1 3 5) (element 2 2 3) (element 3 11 17)))
;            #(combination
;              0
;              #(lambda
;                   0
;                 #(combination
;                   1
;                   #(lambda
;                        1
;                      #(combination
;                        1
;                        #(closure
;                          1
;                          ((0 . 1))
;                          #(sequence
;                             2
;                             (#(setter (1 . 1) #(reference/parameter 1))
;                              #(combination
;                                0
;                                #(closure
;                                  0
;                                  ((1 . 1))
;                                  #(and
;                                    #(combination
;                                      3
;                                      #(reference/closed 1)
;                                      (#(constant/generate 1) #(constant/generate 3) #(constant/generate 5)))
;                                    #(and
;                                      #(combination
;                                        3
;                                        #(reference/closed 1)
;                                        (#(constant/generate 2) #(constant/generate 2) #(constant/generate 3)))
;                                      #(combination
;                                        3
;                                        #(reference/closed 1)
;                                        (#(constant/generate 3) #(constant/generate 11) #(constant/generate 17))))))
;                                ()))))
;                        (#(lambda
;                              3
;                            #(sequence
;                               2
;                               (#(combination
;                                  1
;                                  #(reference/global display)
;                                  (#(combination
;                                     2
;                                     #(reference/global format)
;                                     (#(constant/generate "element: ~a\n") #(reference/parameter 1)))))
;                                #(combination 2 #(reference/global <) (#(reference/parameter 2) #(reference/parameter 3)))))))))
;                   (#(constant/generate undefined))))
;              ()))
;            (x (mischief/recompile/unit code)))
;      (should-be 'recompile/and/2 #t (start x))))
;
;  (recompile/and/1)
;  (recompile/and/2))
;
;
;(define (test/recompile/or)
;  (define (recompile/or/1)
;    (let* ((code
;            ; Only elements 1 and 2 should execute.
;            ;(let ()
;            ;  (define (element n a b)
;            ;    (display (format "element: ~a\n" n))
;            ;    (< a b))
;            ;  (or (element 1 5 3) (element 2 3 5) (element 3 7 8))))))
;            #(combination
;              0
;              #(lambda
;                   0
;                 #(combination
;                   1
;                   #(lambda
;                        1
;                      #(combination
;                        1
;                        #(closure
;                          1
;                          ((0 . 1))
;                          #(sequence
;                             2
;                             (#(setter (1 . 1) #(reference/parameter 1))
;                              #(combination
;                                0
;                                #(closure
;                                  0
;                                  ((1 . 1))
;                                  #(or
;                                    #(combination
;                                      3
;                                      #(reference/closed 1)
;                                      (#(constant/generate 1) #(constant/generate 5) #(constant/generate 3)))
;                                    #(or
;                                      #(combination
;                                        3
;                                        #(reference/closed 1)
;                                        (#(constant/generate 2) #(constant/generate 3) #(constant/generate 5)))
;                                      #(combination
;                                        3
;                                        #(reference/closed 1)
;                                        (#(constant/generate 3) #(constant/generate 7) #(constant/generate 8))))))
;                                ()))))
;                        (#(lambda
;                              3
;                            #(sequence
;                               2
;                               (#(combination
;                                  1
;                                  #(reference/global display)
;                                  (#(combination
;                                     2
;                                     #(reference/global format)
;                                     (#(constant/generate "element: ~a\n") #(reference/parameter 1)))))
;                                #(combination 2 #(reference/global <) (#(reference/parameter 2) #(reference/parameter 3)))))))))
;                   (#(constant/generate undefined))))
;              ()))
;           (x (mischief/recompile/unit code)))
;      
;      (should-be 'recompile/or/1 #t (start x))))
;  
;  (define (recompile/or/2)
;    (let* ((code
;            ; All elements, 1 thru 3, should execute.
;            ;(let ()
;            ;  (define (element n a b)
;            ;    (display (format "element: ~a\n" n))
;            ;    (< a b))
;            ;  (or (element 1 5 3) (element 2 3 2) (element 3 11 17))))))
;            #(combination
;              0
;              #(lambda
;                   0
;                 #(combination
;                   1
;                   #(lambda
;                        1
;                      #(combination
;                        1
;                        #(closure
;                          1
;                          ((0 . 1))
;                          #(sequence
;                             2
;                             (#(setter (1 . 1) #(reference/parameter 1))
;                              #(combination
;                                0
;                                #(closure
;                                  0
;                                  ((1 . 1))
;                                  #(or
;                                    #(combination
;                                      3
;                                      #(reference/closed 1)
;                                      (#(constant/generate 1) #(constant/generate 5) #(constant/generate 3)))
;                                    #(or
;                                      #(combination
;                                        3
;                                        #(reference/closed 1)
;                                        (#(constant/generate 2) #(constant/generate 3) #(constant/generate 2)))
;                                      #(combination
;                                        3
;                                        #(reference/closed 1)
;                                        (#(constant/generate 3) #(constant/generate 11) #(constant/generate 17))))))
;                                ()))))
;                        (#(lambda
;                              3
;                            #(sequence
;                               2
;                               (#(combination
;                                  1
;                                  #(reference/global display)
;                                  (#(combination
;                                     2
;                                     #(reference/global format)
;                                     (#(constant/generate "element: ~a\n") #(reference/parameter 1)))))
;                                #(combination 2 #(reference/global <) (#(reference/parameter 2) #(reference/parameter 3)))))))))
;                   (#(constant/generate undefined))))
;              ()))
;           (x (mischief/recompile/unit code)))
;
;      (should-be 'recompile/or/2 #t (start x))))
;
;  (recompile/or/1)
;  (recompile/or/2))
;
;
;(define (test/recompile/when)
;  (define (recompile/when/1)
;    (let* ((code #(when #(constant/generate #t) #(constant/generate hello))) ; (when #t 'hello)
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/when/1 'hello (start x))))
;
;  (define (recompile/when/2)
;    (let* ((code #(when #(constant/generate #f) #(constant/generate wrong))) ; (when #f 'wrong)
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/when2 (void) (start x))))
;
;  (recompile/when/1)
;  (recompile/when/2))
;
;(define (test/recompile/unless)
;  (define (recompile/unless/1)
;    (let* ((code #(unless #(constant/generate #f) #(constant/generate hello))) ; (unless #f 'hello)
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/unless/1 'hello (start x))))
;  
;  (define (recompile/unless/2)
;    (let* ((code #(unless #(constant/generate #t) #(constant/generate wrong))) ; (unless #t 'wrong)
;           (x (mischief/recompile/unit code)))
;      (should-be 'recompile/unless/2 (void) (start x))))
;
;
;  (recompile/unless/1)
;  (recompile/unless/2))
  
;; Return #t if v is a primitive unstructured value and #f otherwise.
(define (value/primitive? v)
  (or (boolean? v)
      (number? v)
      (char? v)
      (symbol? v)
      (string? v)
      (bytes? v)
      (null? v)
      (void? v)))

(define (pairs/cycle? start)
  (cond
    [(null? start) #f]
    [else (helper/pairs/cycle? start (cdr start))]))

(define (helper/pairs/cycle? slow fast)
  (cond
   [(null? fast) #f]
   [(null? (cdr fast)) #f]
   [(pair? (car slow))
    (helper/pairs/cycle? (car slow) (cdar slow))]
   [(vector? (car slow))
    (let ((v (vector->list (car slow))))
      (helper/pairs/cycle? v (cdr v)))]
   [(eq? (car slow) (car fast)) #t]
   [else (helper/pairs/cycle? (cdr slow) (cddr fast))]))

;(define (any/cycle? x translation)
;  (cond
;    ((value/primitive? x) #f)
;
;    ((pair? x)
;     (pairs/cycle? x))
;
;    ((vector? x)
;     (if translation
;         (if (hash-has-key? translation x)
;             (any/cycle? (hash-ref translation x) translation)
;
;             (let ((v (vector->list x)))
;               (hash-set! translation x v)
;               (any/cycle? v translation)))
;
;         (any/cycle? x (make-hasheq)))
;     
;
;
;       
;     (pairs/cycle? (vector->list x)))
;    ((box? x)
;     (any/cycle? (unbox x)))
;    ((hash? x)
;     (hash-for-each x (lambda (k v) (or (any/cycle? k) (any/cycle? v)))))))
     
;(define (share-id share cycle)
;  (+ (hash-count share)
;     (hash-count cycle)))
;
;(define (is-mutable? o)
;  (and (or (mpair? o)
;           (box? o)
;           (vector? o)
;           (hash? o))
;       (not (immutable? o))))

