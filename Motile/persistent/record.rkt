#lang racket/base

(require
 (only-in racket/vector vector-copy)
 (only-in "../persistent/hash.rkt" hash/eq/null hash/keys hash/ref hash/contains?))

(provide
 ;; The following functions are exported for installation into the Motile baseline.
 record?
 record/contains?
 record/signed?
 record/kind
 record/keys
 record/unsign
  
 ;; The following functions are used only in Racket infrastructure code supporting Motile.
 ;; In particular (record ...0 and (record/cons ...) are implemented as special forms in the Motile compiler.
 record/raw/kind
 record/raw/hash
 record/raw/signature
 error/field/unknown
 error/unrecord)
 

;; Self-describing record sructure based on persistent hash tables.
;; #(<record> <kind> <hash> <signature>)

(define-syntax-rule (record/raw/kind  r)     (vector-ref r 1))
(define-syntax-rule (record/raw/hash  r)     (vector-ref r 2))
(define-syntax-rule (record/raw/signature r) (vector-ref r 3))

(define (record? r)
  (and (vector? r)
       (= (vector-length r) 4)
       (eq? (vector-ref r 0) '<record>)))

(define (record/signed? r)
  (bytes? (record/raw/signature r)))

;; Produce a copy of record r that is unsigned.
(define (record/unsign r)
  (if (not (record/signed? r))
      r
      (let ((u (vector-copy r)))
        (vector-set! u 3 #f)
        u)))

;; Return the kind of record r as a symbol.
(define (record/kind r)
  (if (record? r)
      (record/raw/kind r)
      (error/unrecord 'record/kind r)))

;; Return the field names of record r as symbols in a tuple.
(define (record/keys r)
  (if (record? r)
      (hash/keys (record/raw/hash r))
      (error/unrecord 'record/keys r)))

(define (record/contains? r tag)
  (if (record? r)
      (hash/contains? (record/raw/hash r) tag)
      #f))

;; The given field tag is unknown in record r.
(define (error/field/unknown use r field)
  (error use "no such field ~s in record type ~s" field (record/raw/kind r)))

;; Raise an error as x is not a record.
(define (error/unrecord use x)
  (error use "not a record ~s" x))

;; All of this is an emulation of Racket records in the base Scheme.

;(define-syntax record/new
;  (syntax-rules ()
;    [(_ name (field value) ...)
;     (vector '<record> 'name (vector 'field ...) value ...)]))
;
;(define-syntax-rule (:: r field)
;  (cond
;    ((vector-memq 'field (record/tags r))
;     => (lambda (i) (vector-ref r (+ 3 i))))
;    (else (error/field/unknown 'record/ref r 'field))))
;  
;(define-syntax-rule (::? r field failure)
;  (cond
;    ((vector-memq 'field (record/tags r))
;     => (lambda (i) (vector-ref r (+ 3 i))))
;    (else failure)))
;
;(define (record/cons/field r field value)
;  (cond
;    ((vector-memq field (record/tags r))
;     => (lambda (i)
;          (let ((v (vector-copy r)))
;            (vector-set! v (+ 3 i) value)
;            v)))
;    (else (error/field/unknown 'record/cons r field))))
;
;(define (record/cons/fields r fields values)
;  (let loop ((v (vector-copy r)) (i 0) (n (vector-length fields)))
;    (if (< i n)
;        (cond
;          ((vector-memq (vector-ref fields i) (record/tags r))
;           => (lambda (j)
;                (vector-set! v (+ 3 j) (vector-ref values i))
;                (loop v (add1 i) n)))
;          (else (error/field/unknown 'record/cons r (vector-ref fields i))))
;        v)))
;
;(define-syntax record/cons
;  (syntax-rules ()
;    [(_ r (field value) ...) (record/cons/fields r (vector 'field ...) (vector value ...))]
;    [(_ r field value) (record/cons/field r 'field value)]))
