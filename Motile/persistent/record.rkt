#lang racket/base

(require
 (only-in racket/vector vector-copy vector-memq)
 racket/match
 (only-in "../persistent/tuple.rkt" vector/tuple))

(provide
 ;; The following three functions are exported for installation into the Motile baseline.
 record?
 record/kind
 record/fields
 ;; The following functions are used only in Racket infrastructure code supporting Motile.
 ;; In particular record/new and record/cons are implemented as special forms in the Motile compiler.
 record/tags
 record/new
 ::
 ::?
 record/cons
 error/field/unknown
 error/unrecord)
 

;; Simple, self-describing record structure that is well-suited for serialization and mobility,
;; and can be easily expressed in other langauges. Field access is linear in the number of fields.

(define-syntax-rule (record/magic r) (vector-ref r 0))
(define-syntax-rule (record/name  r) (vector-ref r 1))
(define-syntax-rule (record/tags  r) (vector-ref r 2))

(define (record? r)
  (if (vector? r)
      (let ((n (vector-length r)))
        (and
         (> n 2)
         (eq?     (record/magic r) '<record>)
         (symbol? (record/name r)) ; The name of the record type.
         (vector? (record/tags r)) ; The fields of the record.
         (= (- n 3) (vector-length (record/tags r))))) ; We have as many values as fields.
      #f))

;; Return the kind of record r as a symbol.
(define (record/kind r)
  (if (record? r)
      (record/name r)
      (error/unrecord 'record/kind r)))

;; Return the field names of record r as symbols in a tuple.
(define (record/fields r)
  (if (record? r)
      (vector/tuple (record/tags r))
      (error/unrecord 'record/field r)))

;; The given field tag is unknown in record r.
(define (error/field/unknown use r field)
  (error use "no such field ~s in record type ~s" field (record/name r)))

;; Raise an error as x is not a record.
(define (error/unrecord use x)
  (error use "not a record ~s" x))

;; All of this is an emulation of Racket records in the base Scheme.

(define-syntax record/new
  (syntax-rules ()
    [(_ name (field value) ...)
     (vector '<record> 'name (vector 'field ...) value ...)]))

(define-syntax-rule (:: r field)
  (cond
    ((vector-memq 'field (record/tags r))
     => (lambda (i) (vector-ref r (+ 3 i))))
    (else (error/field/unknown 'record/ref r 'field))))
  
(define-syntax-rule (::? r field failure)
  (cond
    ((vector-memq 'field (record/tags r))
     => (lambda (i) (vector-ref r (+ 3 i))))
    (else failure)))

(define (record/cons/field r field value)
  (cond
    ((vector-memq field (record/tags r))
     => (lambda (i)
          (let ((v (vector-copy r)))
            (vector-set! v (+ 3 i) value)
            v)))
    (else (error/field/unknown 'record/cons r field))))

(define (record/cons/fields r fields values)
  (let loop ((v (vector-copy r)) (i 0) (n (vector-length fields)))
    (if (< i n)
        (cond
          ((vector-memq (vector-ref fields i) (record/tags r))
           => (lambda (j)
                (vector-set! v (+ 3 j) (vector-ref values i))
                (loop v (add1 i) n)))
          (else (error/field/unknown 'record/cons r (vector-ref fields i))))
        v)))

(define-syntax record/cons
  (syntax-rules ()
    [(_ r (field value) ...) (record/cons/fields r (vector 'field ...) (vector value ...))]
    [(_ r field value) (record/cons/field r 'field value)]))

;(define-match-expander tuple
;  (syntax-rules ()
;    [(_ a ...)
;     (vector '<tuple> a ...)]))


