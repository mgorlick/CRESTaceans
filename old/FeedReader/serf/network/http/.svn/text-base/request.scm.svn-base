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

;; <http/request> method declarations.
(define http/request           (make-generic-method))
(define http/request/encoding  (make-generic-method))
(define http/request/encoding! (make-generic-method))
(define http/request/entity    (make-generic-method))
(define http/request/entity!   (make-generic-method))
(define http/request/headers   (make-generic-method))
(define http/request/headers!  (make-generic-method))
(define http/request/header!   (make-generic-method))
(define http/request/header?   (make-generic-method))
(define http/request/header    (make-generic-method))
(define http/request/length    (make-generic-method))
(define http/request/content/length)
(define http/request/method    (make-generic-method))
(define http/request/remove!   (make-generic-method))
(define http/request/type      (make-generic-method))
(define http/request/content/type)
(define http/request/type/exploded (make-generic-method)) ; Remove this. Obsoleted by http/header/parse and friends.
(define http/request/type!     (make-generic-method))
(define http/request/uri       (make-generic-method))
(define http/request/uri*      (make-generic-method))
(define :null-java-http-request:)

;; Need this here for :null-java-http-request: above.
(define-java-class <java-basic-http-request> |org.apache.http.message.BasicHttpRequest|)

;; Class definition.
(define <http/request>
  (let ()
    ; Java definitions.
    (define-java-class <java-basic-http-entity-request> |org.apache.http.message.BasicHttpEntityEnclosingRequest|)
    (define-java-class <java-string-entity>             |org.apache.http.entity.StringEntity|)
    (define-java-class <java-http-entity>               |org.apache.http.entity.AbstractHttpEntity|)
    (define-generic-java-method contains-header) ; boolean containsHeader(String) for HttpRequest
    (define-generic-java-method get-all-headers) ; Header[] getAllHeaders() for HttpRequest
    (define-generic-java-method get-content-encoding) ; Header getContentEncoding() for HttpEntity
    (define-generic-java-method get-content-length) ; long getContentLength() for HttpEntity
    (define-generic-java-method get-content-type) ; Header getContentType() for HttpEntity
    (define-generic-java-method get-entity) ; HttpEntity getEntity() for HttpRequest
    (define-generic-java-method get-first-header) ; HttpHeader getHeader(String) for HttpRequest
    (define-generic-java-method get-request-line) ; RequestLine() for HttpRequest
    (define-generic-java-method get-method) ; String getMethod() for RequestLine
    (define-generic-java-method get-uri) ; String getUri() for RequestLine
    (define-generic-java-method remove-headers) ; void removeHeader(String) for HttpRequest
    (define-generic-java-method set-content-encoding) ; void setContentEncoding(String) for HttpEntity
    (define-generic-java-method set-content-type) ; void setContentType(String) for HttpEntity
    (define-generic-java-method set-entity) ; void setEntity(HttpEntity) for HttpRequest
    (define-generic-java-method set-header) ; void setHeader(String, String) for HttpRequest
    
    ; Destructively rewrite a list of Headers (h_0 h_1 ... h_{n-1}) in place converting
    ; it into an association list of (name . value) pairs.
    ; head: the head of the list of headers
    ; rest: the remainder of the list of headers as we cdr our way down
    (define (translate-headers head rest)
      (cond
       ((null? rest) head)
       (else
	(set-car! rest (http/header/unwrap (car rest))) ; Destructive rewrite.
	(translate-headers head (cdr rest)))))

    ; Returns #t if e is an HttpEntity of some form and #f otherwise.
    (define (http-entity? e)
      (and
       (java-object? e)
       (instance-of? e <java-http-entity>)))

    ; Returns #t if r is a BasicHttpRequest and #f otherwise.
    (define (basic-request? r)
      (and
       (java-object? r)
       (instance-of? r <java-basic-http-request>)))

    ; Returns #t if r is a BasicHttpEntityEnclosingRequest and #f otherwise.
    (define (basic-entity-request? r)
      (and
       (java-object? r)
       (instance-of? r <java-basic-http-entity-request>)))

    ; Internal helper that creates a Java BasicHttpRequest or BasicHttpEntityEnclosingRequest.
    ; method: HTTP request method as string, symbol or Java String
    ; uri:    HTTP request URI as string or Java String
    ; entity: #t if request requires an entity body and #f if no entity body required.
    (define (http/request/new method uri entity)
      (let ((method
	     (if (or (string? method) (symbol? method)) (->jstring method) method))
	    (uri 
	     (if (string? uri) (->jstring uri) uri)))
	(java-new
	 (if entity <java-basic-http-entity-request> <java-basic-http-request>)
	 method uri)))

    ; Actual class definition.
    (lambda ()
      (let ((self (make-object))
	    (request #f)  ; Either a BasicHttpRequest or a BasicHttpEntityEnclosingRequest instance.
	    (entity  #f)) ; The entity body (if any).

	(make-method!
	 self instantiate
	 ; method: string or symbol
	 ; uri: string
	 ; e: string or StringEntity
	 (lambda (self . arguments)
	   (match
	    arguments
	    ((,method ,uri)
	     (set! request (http/request/new method uri #f)))

	    ((,method ,uri ,e)
	     (cond
	      ((string? e)
	       (set! request (http/request/new method uri #t)) ; BasicHttpEntityEnclosingRequest (with entity).
	       (set! entity (http/entity/new e))
	       (set-entity request entity))

	      ((http-entity? e)
	       (set! request (http/request/new method uri #t))
	       (set! entity e)
	       (set-entity request entity))))

	    ((,java-request) ; BasicHttpEntityEnclosingRequest is subclass of BasicHttpRequest.
	     (cond
	      ((basic-entity-request? java-request) ; Test for subclass must precede test for superclass.
	       (set! request java-request)
	       (let ((e (get-entity java-request)))
		 (if (not (java-null? e))
		     (set! entity e))))

	      ((basic-request? java-request)
	       (set! request java-request)))))))

	; Return an association list of all headers in the order they
	; will appear in the HTTP request.
	(make-method!
	 self http/request/headers
	 (lambda (self)
	   (let ((headers (->list (get-all-headers request))))
	     (translate-headers headers headers))))

	(make-method!
	 self http/request/headers!
	 (lambda (self headers) ; headers: association list of (name . value) pairs.
	   (for-each
	    (lambda (pair)
	      (let* ((name (car pair))
		     (value (cdr pair)))
		(set-header
		 request
		 (->jstring name)
		 (->jstring (if (string? value) value (format "~a" value))))))
	    headers)))

	(make-method!
	 self http/request/method
	 (lambda (self)
	   (->string (get-method (get-request-line request)))))

	(make-method!
	 self http/request/uri ; Return the request URI as a Scheme string.
	 (lambda (self)
	   (->string (get-uri (get-request-line request)))))

	(make-method!
	 self http/request/uri* ; Like http/request/uri except return a Java String.
	 (lambda (self)
	   (get-uri (get-request-line request))))

	(make-method!
	 self http/request/header! ; Set header to name/value pair.
	 (lambda (self . arguments) ; (http/request/header! self (name . value)) or (http/request/header! self name value).
	   (match
	    arguments
	    (((,name . ,value))
	     (set-header request (->jstring name) (->jstring value))
	     #t)
	    ((,name ,value)
	     (set-header request (->jstring name) (->jstring value))
	     #t)
	    (_ #f))))

	(make-method!
	 self http/request/remove! ; remove the named header from the request.
	 (lambda (self name)
	   (remove-headers request (->jstring name))
	   #t))

	(make-method!
	 self http/request/header? ; Returns #t if header (name . value) is in request and #f otherwise.
	 (lambda (self name) (->boolean (contains-header request (->jstring name)))))

	(make-method!
	 self http/request/header ; Returns (name . value) if header is present and #f otherwise.
	 (lambda (self name)
	   (let ((h (get-first-header request (->jstring name))))
	     (if (java-null? h) #f (http/header/unwrap h)))))

	(make-method!
	 self http/request/entity ; Return the Java entity (if any).
	 (lambda (self) entity))

	(make-method!
	 self http/request/entity!
	 (lambda (self e)
	   (set! entity (http/entity/new e))
	   (set-entity request entity)))

	(make-method!
	 self http/request ; Return the Java request.
	 (lambda (self) request))

	(make-method!
	 self http/request/length ; Return the content length or #f if no content.
	 (lambda (self)
	   (cond
	    ((and entity (get-content-length entity)) =>
	     (lambda (n)
	       (if (java-null? n) #f (->number n))))
	    (else #f))))

	(make-method!
	 self http/request/encoding
	 (lambda (self)
	   (cond
	    ((and entity (get-content-encoding entity)) =>
	     (lambda (h)
	       (if (java-null? h) #f (http/header/unwrap h))))
	    (else #f))))

	(make-method!
	 self http/request/encoding!
	 (lambda (self e) ; e: a Scheme string, association (name . value) or #f.
	   (if entity
	       (set-content-encoding
		entity
		(cond
		 ((string? e) (->jstring e))
		 ((pair? e)   (->jstring (cdr e)))     ; e is (name . value) pair.
		 (else        :java-null-string:)))))) ; Reset to null.

	(make-method!
	 self http/request/type ; Return the Content-Type header (if any) as a pair ("Content-Type" . value).
	 (lambda (self)
	   (cond
	    ((and entity (get-content-type entity)) =>
	     (lambda (h)
	       (if (java-null? h) #f (http/header/unwrap h))))
	    (else #f))))

	(make-method!
	 self http/request/type/exploded
	 (lambda (self)
	   (cond
	    ((and entity (get-content-type entity)) =>
	     (lambda (h)
	       (if (java-null? h)
		   #f
		   (let* ((pair (http/header/unwrap h))
			  (tokens (string-tokenize (cdr pair)))
			  (mime (car tokens))
			  (parameters (parameters-to-association-list (cdr tokens))))
		     (cons mime parameters)))))
	    (else #f))))


	(make-method!
	 self http/request/type!
	 (lambda (self e) ; e: a Scheme string, association (name . value) or #f.
	   (if entity
	       (set-content-type
		entity
		(cond
		 ((string? e) (->jstring e))
		 ((pair? e)   (->jstring e (cdr e))) ; e is (name . value) pair.
		 (else        :java-null-string:)))))) ; Reset to null.

	self))))

;; Break a parameter string "name=value" into (name . value).
(define (crack-parameter parameter)
  (let ((i (string-index parameter #\=)))
    (if i
	(cons (substring/shared parameter s 0 (- i 1))
	      (substring/shared parameter s (+ i 1)))
	#f)))

(define (crack-parameters parameters) #f)

(define (request/test/01)
  (let ((r (object/forge <http/request> "GET" "/")))
    (display (format "request: ~a\n" (http/request r)))
    (display (format "entity: ~a\n" (http/request/entity r)))
    (display (format "method: ~a\n" (http/request/method r)))
    (display (format "uri: ~a\n" (http/request/uri r)))

    (http/request/header! r "foo" "1111")
    (http/request/header! r (cons "barz" "2222"))
    (http/request/header! r "If-Modified" "run-away")
    r))

(define (request/test/02)
  (let ((r (object/forge <http/request> "POST" "/" "this is a small bit of content")))
    (display (format "request: ~a\n" (http/request r)))
    (display (format "entity: ~a\n" (http/request/entity r)))
    (display (format "method: ~a\n" (http/request/method r)))
    (display (format "uri: ~a\n" (http/request/uri r)))

    (http/request/header! r "foo" "1111")
    (http/request/header! r (cons "barz" "2222"))
    (http/request/header! r "If-Modified" "run-away")
    r))

(set! :null-java-http-request: (java-null <java-basic-http-request>))
(set! http/request/content/length http/request/length)
(set! http/request/content/type   http/request/type)