#lang racket/base

(provide
 island/address/new
 island/address?
 island/address/ok?
 island/address/equal?
 
 ; Fields of island structure.
 island/address/public
 island/address/dns
 island/address/port
 
 this/island)

;; signing - self-certifying signing of island public key, DNS, and IP port number as a byte string.
;; DNS - DNS name as byte string
;; port - IP port number as integer
(define (island/address/new public dns port) 
  (or (bytes? public) (raise-type-error 'island/address/new "?bytes" public))
  (or (bytes? dns)    (raise-type-error 'island/address/new "?bytes" dns))
  (or (and (integer? port) (positive? port)) (raise-type-error 'island/address/new "?integer/positive" port))
  (vector '<island> public dns port))

;; Returns #t if x is of type <island> and #f otherwise.
(define (island/address? x)
  (and
   (vector? x)
   (= (vector-length x) 4)
   (eq? (vector-ref x 0) '<island>)))

;; Returns #t if the fields of island/address x are type correct and #f otherwise.
(define (island/address/ok? x)
  (and
   (let ((p (island/address/public x)))
     (and (bytes? p) (positive? (bytes-length p))))
   
   (let ((d (island/address/dns x)))
     (and (bytes? d) (positive? (bytes-length d))))

   (let ((p (island/address/port x)))
     (and (integer? p) (positive? p)))))

(define-syntax-rule (island/address/public x) (vector-ref x 1)) ; Public key (as bytes string).
(define-syntax-rule (island/address/dns x)    (vector-ref x 2)) ; DNS name (as bytes string).
(define-syntax-rule (island/address/port x)   (vector-ref x 3)) ; IP port number.

;; Returns #t if island/address x is equal to island address y.
(define (island/address/equal? x y)
  (and
   (island/address? x) (island/address? y)
   (bytes=? (island/address/dns x) (island/address/dns y))
   (= (island/address/port x) (island/address/port y))
   (bytes=? (island/address/public x) (island/address/public y))))

(define this/island (make-parameter #f))

