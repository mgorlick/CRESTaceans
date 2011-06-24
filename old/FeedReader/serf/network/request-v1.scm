;; Copyright 2009 Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; A Scheme wrapper around the Apache HTTP Components BasicHttpRequest and BasicHttpEntityEnclosingRequest.

(define-generic-java-method get-all-headers) ; Header[] getAllHeaders() for HttpRequest
(define-generic-java-method get-request-line) ; RequestLine() for HttpRequest
(define-generic-java-method get-method) ; String getMethod() for RequestLine
(define-generic-java-method get-uri) ; String getUri() for RequestLine
(define-generic-java-method get-entity) ; HttpEntity getEntity() for HttpRequest
(define-java-class <java-http-entity-request> |org.apache.http.message.BasicHttpEntityEnclosingRequest|)

(define http/request/method   (make-generic-method))
(define http/request/uri      (make-generic-method))
(define http/request/length   (make-generic-method))
(define http/request/type     (make-generic-method))
(define http/request/encoding (make-generic-method))
(define http/request/entity   (make-generic-method))

;; Destructively rewrite a list of Headers (h_0 h_1 ... h_{n-1}) in place converting
;; it into an association list of (name . value) pairs.
;; head: the head of the list of headers
;; rest: the remainder of the list of headers as we cdr our way down
(define (translate-headers head rest)
  (cond
   ((null? rest) head)
   (else
    (set-car! rest (http/header/unwrap (car rest))) ; Destructive rewrite.
    (translate-headers head (cdr rest)))))

(define (<http/request>)
  (let ((self    (make-object))
	(headers #f)
	(method  #f)
	(uri     #f)
	; Content.
	(length  #f)
	(type     #f)
	(encoding #f)
	(entity   #f)
	(writable #f) ; #t iff may be modified.
    

    (make-method!
     self instantiate
     (lambda (self . arguments)
       (if (null? arguments)
	   (set! headers '())
	   (instantiate/nonvirgin self (car arguments)))))

    ; java-http-request: an instance of <java-http-request> or <java-http-entity-request>
    (make-method!
     self instantiate/nonvirgin
     (lambda (self java-http-request)
       (set! headers (->list (get-all-headers java-http-request)))
       (translate-headers headers headers)

       (let ((java-request-line (get-request-line java-http-request)))
	 (set! method (java-to-string (get-method java-request-line)))
	 (set! uri    (java-to-string (get-uri    java-request-line))))

       (if (instance-of? java-http-request <java-http-entity-request>)
	   (let* ((e (get-entity java-http-request))
		  (n (->number (get-content-length e)))
		  (t (get-content-type e))) ; Returns a Header or java-null.
	     (set! content-length (if (> n 0) n #f))
	     (set! content-type (if (java-null? t) #f (java-to-string (get-value t))))
	     (set! entity (if (and content-length (not (java-null? e))) (http/entity/to-string e) #f))))))

    (make-method!
     self http/request/headers
     (lambda (self) headers))

    (make-method!
     self http/request/method
     (lambda (self) method))

    (make-method!
     self http/request/uri
     (lambda (self) uri))

    (make-method!
     self http/request/length
     (lambda (self) content-length))

    (make-method!
     self http/request/type
     (lambda (self) content-type))

    (make-method!
     self http/request/entity
     (lambda (self) entity))

    self))