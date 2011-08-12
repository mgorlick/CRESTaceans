#lang racket/base

; Utility routines used to construct the BASELINE and other principal island environs.

(require
 (only-in
  "frame.rkt"
  arguments/pack
  arguments/list/pack
  a/1 a/2 a/3
  a/EMPTY
  arity/verify
  vector/list/copy!)
(only-in
 "utility.rkt"
 decompile?
 error/motile/internal/call
 k/RETURN))

(provide
 define/combinator/2
 define/combinator/3
 define/global/0
 define/global/1
 define/global/2
 define/global/3
 define/global/N
 descriptor/global
 motile/call
 motile/call/3
 motile/decompile)
 
(define (descriptor/global symbol)
  (vector-immutable 'reference/global symbol))

;; Many of the persistent functional data structures implement combinators of the form (C d u) where:
;;    C is a combinator of the persistent functional data structure,
;;    d is an instance of the data structure, and 
;;    u a function applied by the combinator.
;; These combinators must be wrapped before they can be used within Motile.

;; Wrapper for combinator that takes two arguments:
;; a data structure instance and a single-argument Motile function.
;; symbol - Motile name for the combinator being wrapped
;; combinator - Racket implementation of the combinator
(define (motile/combinator/2 symbol combinator)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (arity/verify a 2 symbol)
         (let* ((instance (a/1  a)) ; Data structure instance.
                (f        (a/2 a)) ; Motile function to be applied by combinator.
                (h (lambda (x) (f k/RETURN (arguments/pack x) g))))
           (k (combinator instance h))))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; Wrapper for combinator that takes three arguments:
;; a data structure instance, a two-argument Motile function, and a seed value.
;; symbol - Motile name for the combinator being wrapped
;; combinator - Racket implementation of the combinator
(define (motile/combinator/3 symbol combinator)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (arity/verify a 3 symbol)
         (let* ((instance (a/1  a))
                (f        (a/2 a))
                (seed     (a/3  a))
                (h (lambda (x y) (f k/RETURN (arguments/pack x y) g))))
           (k (combinator instance h seed))))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; The following helper functions: motile/global/0, ..., motile/global/3
;; wrap host Scheme functions so that they can be called from Motile programs.

;; Wrapper for zero-argument host procedures.
(define (motile/global/0 symbol procedure)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (arity/verify a 0 symbol)
         (k (procedure)))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; Wrapper for one-argument host procedures.
(define (motile/global/1 symbol procedure)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (arity/verify a 1 symbol)
         (k (procedure (a/1 a))))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; Wrapper for two-argument host procedures.
(define (motile/global/2 symbol procedure)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (arity/verify a 2 symbol)
         (k (procedure (a/1 a) (a/2 a))))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; Wrapper for three-argument host procedures.
(define (motile/global/3 symbol procedure)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (arity/verify a 3 symbol)
         (k (procedure (a/1 a) (a/2 a) (a/3 a))))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; Wrapper for host procedures that accept more than three arguments
;; or accept a variable number of arguments.
(define (motile/global/N symbol procedure)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (k (apply procedure (cdr (vector->list a)))))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; Convenient helper macros for building global binding environments.
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

;; This is the inverse of the helper macros above.
;; It allows a Scheme host function to a call a Motile function f in the context of global binding environ g.
;; The value returned is the value computed by f.
(define (motile/call f g . arguments)
  ;(f k/RETURN (if (null? arguments) a/EMPTY (arguments/list/pack arguments)) g))
  (f k/RETURN (if (null? arguments) #f (arguments/list/pack arguments)) g))
;; Like motile/call but the arguments to f must be assembled as a list.
(define (motile/call/3 f g arguments)
  ;(f k/RETURN (arguments/list/pack arguments) g))
  (f k/RETURN (if (null? arguments) #f (arguments/list/pack arguments)) g))
;; Given a Motile code closure c returns the decompilation of c as a
;; Motile Assembly Languaage (MAL) graph.
(define (motile/decompile c) (c #f #f #f))
