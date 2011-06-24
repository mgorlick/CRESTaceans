;;; "object.scm" Macroless Object System
;;; Author: Wade Humeniuk <humeniuw@cadvision.com>
;;; This code is in the public domain.

;;;Date:  February 15, 1994

;; Object Construction:
;;       0           1          2             3              4
;; #(object-tag get-method make-method! unmake-method! get-all-methods)

(define :object-flavor-tag: '|132e288d-2079-478a-88ec-b847a19179e4|)

(define (object-remove-eq obj alist)
  (cond
   ((null? alist) alist)
   ((eq? (caar alist) obj) (cdr alist))
   (else (cons (car alist) (object-remove-eq obj (cdr alist))))))

(define (get-all-methods obj)
  (if (object? obj)
      ((vector-ref obj 4))
      (error 'get-all-methods "non-object: " obj)))

(define (object? obj)
  (and (vector? obj)
       (eq? :object-flavor-tag: (vector-ref obj 0))))

;; Affiliate the given method with the given generic method in the object.
(define (make-method! obj generic-method method)
  (if (object? obj)
      (if (procedure? method)
	    (begin
	      ((vector-ref obj 2) generic-method method)
	      method)
	      (error 'make-method! "method must be a procedure: " method))
      (error 'make-method! "non-object: " obj)))

;; Return the object-specific method affliated with the given generic method.
(define (get-method obj generic-method)
  (if (object? obj)
      ((vector-ref obj 1) generic-method)
      (error 'get-method "non-object: " obj)))

;; Remove from the object the object-specific method affiliated with the given generic method.
(define (unmake-method! obj generic-method)
  (if (object? obj)
      ((vector-ref obj 3) generic-method)
      (error 'unmake-method! "non-object: " obj)))

(define (make-predicate! obj generic-predicate)
  (if (object? obj)
      ((vector-ref obj 2) generic-predicate (lambda (self) #t))
      (error 'make-predicate! "non-object: " obj)))

;; Construct a generic method that serves to both name (identify) and execute
;; the method of an object.
(define (make-generic-method . exception-procedure)
  (define generic-method
    (lambda (obj . operands)
      (if (object? obj)
	  (let ((object-method ((vector-ref obj 1) generic-method)))
	    (if object-method
		(apply object-method (cons obj operands))
		(error 'generic-method "method not supported: " obj)))
	  (apply exception-procedure (cons obj operands)))))

  (if (not (null? exception-procedure))
      (if (procedure? (car exception-procedure))
	  (set! exception-procedure (car exception-procedure))
	  (error 'make-generic-method "exception handler not procedure: " (car exception-procedure)))
      (set! exception-procedure
	    (lambda (obj . params)
	      (error 'make-generic-method "operation not supported: " obj))))
  generic-method)

(define (make-generic-predicate)
  (define generic-predicate
    (lambda (obj)
      (if (object? obj)
	  (if ((vector-ref obj 1) generic-predicate) #t #f)
	  #f)))
  generic-predicate)

;; Returns an object (that is, its underlying vector representation) whose method list is the
;; concatenation of the method lists of the ancestors in ancestor order from left to right.
;; Any additional methods are added to this object.
(define (make-object . ancestors)
  (define method-list
    (apply append (map (lambda (obj) (get-all-methods obj)) ancestors)))
  (define (make-method! generic-method method)
    (set! method-list (cons (cons generic-method method) method-list))
    method)
  (define (unmake-method! generic-method)
    (set! method-list (object-remove-eq generic-method method-list))
    #t)
  (define (all-methods) method-list)
  (define (get-method generic-method)
    (let ((method-def (assq generic-method method-list)))
      (if method-def (cdr method-def) #f)))
  (vector :object-flavor-tag: get-method make-method! unmake-method! all-methods))

;; Generic initialization.
(define instantiate (make-generic-method))

(define (object/forge prototype . arguments)
  (let ((x (prototype)))
    (apply instantiate (cons x arguments))
    x))

;; Example
#|(require-library 'sisc/libs/srfi/srfi-13) ; String library.
(import* srfi-13 string-join)

(define uri/host (make-generic-method))
(define uri/port (make-generic-method))
(define uri/path (make-generic-method))
(define uri/query (make-generic-method))
(define uri/parameters (make-generic-method))
(define uri/add-parameter (make-generic-method))
(define uri/path+query (make-generic-method))
(define instantiate (make-generic-method))

(define (<uri>)
  (let ((self (make-object))
	(host #f)
	(port #f)
	(path #f)
	(parameters '()))
    (make-method!
     self instantiate
     (lambda (self $host $port $path . $parameters)
       (set! host $host)
       (set! port $port)
       (set! path $path)
       (set! parameters $parameters)))

    (make-method!
     self uri/host
     (lambda (self) host))

    (make-method!
     self uri/port
     (lambda (self) port))

    (make-method!
     self uri/path
     (lambda (self) path))

    (make-method!
     self uri/parameters
     (lambda (self) parameters))

    (make-method!
     self uri/add-parameter
     (lambda (self $parameter) (set! parameters (cons $parameter parameters))))

    (make-method!
     self uri/query
     (lambda (self)
       (let loop ((pairs parameters)
		  (chunks '()))
	 (cond
	  ((null? pairs)
	   (if (null? chunks) #f (string-join chunks ";")))
	  (else
	   (let ((pair (car pairs)))
	     (loop
	      (cdr pairs)
	      (cons (format "~a=~a" (car pair) (cdr pair)) chunks))))))))

    (make-method!
     self uri/path+query
     (lambda (self)
       (let ((q (uri/query self)))
	 (if q (string-append path "&" q) path))))

    self))

(define (make prototype . arguments)
  (let ((x (prototype)))
    (apply instantiate (cons x arguments))
    x))
|#