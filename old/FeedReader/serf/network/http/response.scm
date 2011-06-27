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

;; A Scheme wrapper around the Apache HTTP Components BasicHttpResponse.

;; Fixes
;;   * Merge http.scm, response.scm and request.scm into a single file/module. There is a lot of
;;     needless duplication among the three.


(define http/response           (make-generic-method))
(define http/response/encoding  (make-generic-method))
(define http/response/encoding! (make-generic-method))
(define http/response/entity    (make-generic-method)) 
(define http/response/entity!   (make-generic-method))
(define http/response/headers   (make-generic-method))
(define http/response/headers!  (make-generic-method))
(define http/response/header!   (make-generic-method))
(define http/response/header?   (make-generic-method))
(define http/response/header    (make-generic-method))
(define http/response/length    (make-generic-method))
(define http/response/content/length)
(define http/response/remove!   (make-generic-method))
(define http/response/reason    (make-generic-method))
(define http/response/reason!   (make-generic-method))
(define http/response/status    (make-generic-method))
(define http/response/status!   (make-generic-method))
(define http/response/type      (make-generic-method))
(define http/response/content/type)
(define http/response/type!     (make-generic-method))

(define <http/response>
  (let ()
    ; Hide the Java grit from view.
    (define-java-class <java-http-response>         |org.apache.http.message.BasicHttpResponse|)
    (define-java-class <java-http-status-line>      |org.apache.http.message.BasicStatusLine|)
    (define-java-class <java-http-protocol-version> |org.apache.http.ProtocolVersion|)
    (define-generic-java-method contains-header)   ; boolean containsHeader(String) for HttpBasicResponse
    (define-generic-java-method get-all-headers)   ; Header[] getAllHeaders() for HttpBasicResponse
    (define-generic-java-method get-content-encoding)  ; Header getContentEncoding() for HttpEntity
    (define-generic-java-method get-content-length)  ; long getContentLength() for HttpEntity
    (define-generic-java-method get-content-type)  ; Header getContentType() for HttpEntity
    (define-generic-java-method get-entity)        ; HttpEntity getEntity() for HttpBasicResponse
    (define-generic-java-method get-first-header)   ; Header getFirstHeader() for HttpBasicResponse
    (define-generic-java-method get-reason-phrase) ; String getReasonPhrase for BasicStatusLine
    (define-generic-java-method get-status-code)   ; int getStatusCode() for BasicStatusLine
    (define-generic-java-method get-status-line)   ; StatusLine getStatusLine() for BasicHttpResponse
    (define-generic-java-method remove-headers)   ; void removeHeaders(String) for HttpBasicResponse
    (define-generic-java-method set-content-encoding) ; setContentEncoding(String) for HttpEntity
    (define-generic-java-method set-content-type) ; setContentEncoding(String) for HttpEntity
    (define-generic-java-method set-entity)        ; void setEntity(HttpEntity) for BasicHttpResponse
    (define-generic-java-method set-header)        ; void setHeader(String, String) for BasicHttpResponse
    (define-generic-java-method set-reason-phrase) ; void setReasonPhrase(String) for BasicHttpResponse
    (define-generic-java-method set-status-code)   ; void setStatusCode(int) for BasicHttpResponse

    (define :http/1.1:)

    (define (http/response/new status reason)
      (java-new <java-http-response> :http/1.1: (->jint status) (->jstring reason)))

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

    (set! :http/1.1: (java-new <java-http-protocol-version> (->jstring "HTTP") (->jint 1) (->jint 1)))

    ; Object constructor.
    (lambda ()
      (let ((self    (make-object))
	    (response #f)
	    (entity   #f))

	(make-method!
	 self instantiate
	 (lambda (self . arguments)
	   (match
	    arguments
	    ((,status ,reason)
	     (set! response (http/response/new status reason)))

	    ((,status ,reason ,e)
	     (cond
	      ((string? e)
	       (set! response (http/response/new status reason))
	       (set! entity   (http/entity/new e))
	       (set-entity response entity))

	      ((http-entity? e)
	       (set! response (http/response/new status reason))
	       (set! entity e)
	       (set-entity response entity))))
  
	    ((,java-response) ; HttpResponse
	     (if (java-object? java-response)
		 (let ((e (get-entity java-response)))
		   (set! response java-response)
		   (if (not (java-null? e))
		       (set! entity e))))))))

	(make-method!
	 self http/response/headers
	 (lambda (self)
	   (let ((headers (->list (get-all-headers response))))
	     (translate-headers headers headers))))

	(make-method!
	 self http/response/header! ; Set header to name/value pair.
	 (lambda (self . arguments) ; (http/response/header! self (name . value)) or (http/response/header! self name value).
	   (match
	    arguments
	    (((,name . ,value))
	     (set-header response (->jstring name) (->jstring value))
	     #t)
	    ((,name ,value)
	     (set-header response (->jstring name) (->jstring value))
	     #t)
	    (_ #f))))

	(make-method!
	 self http/response/headers!
	 (lambda (self headers) ; headers: association list of (name . value) pairs.
	   (for-each
	    (lambda (pair)
	      (let* ((name (car pair))
		     (value (cdr pair)))
		(set-header
		 response
		 (->jstring name)
		 (->jstring (if (string? value) value (format "~a" value))))))
	    headers)))

	(make-method!
	 self http/response/header ; Returns (name . value) if header is present and #f otherwise.
	 (lambda (self name)
	   (let ((h (get-first-header response (->jstring name))))
	     (if (java-null? h) #f (http/header/unwrap h)))))

	(make-method!
	 self http/response/header? ; Returns #t if header (name . value) is in response and #f otherwise.
	 (lambda (self name) (->boolean (contains-header response (->jstring name)))))

	(make-method!
	 self http/response/remove! ; remove the named header from the response.
	 (lambda (self name)
	   (remove-headers response (->jstring name))
	   #t))

	(make-method!
	 self http/response/status
	 (lambda (self)
	   (->number (get-status-code (get-status-line response)))))

	(make-method!
	 self http/response/status!
	 (lambda (self n)
	   (set-status-code response (->jint n))))

	(make-method!
	 self http/response/reason
	 (lambda (self)
	   (->string (get-reason-phrase (get-status-line response)))))

	(make-method!
	 self http/response/reason!
	 (lambda (self r)
	   (set-reason-phrase response (->jstring r))))

	(make-method!
	 self http/response/length ; Return the content length or #f if no content.
	 (lambda (self)
	   (cond
	    ((and entity (get-content-length entity)) =>
	     (lambda (n)
	       (if (java-null? n) #f (->number n))))
	    (else #f))))

	(make-method!
	 self http/response/encoding
	 (lambda (self)
	   (cond
	    ((and entity (get-content-encoding entity)) =>
	     (lambda (h)
	       (if (java-null? h) #f (http/header/unwrap h))))
	    (else #f))))

	(make-method!
	 self http/response/encoding!
	 (lambda (self e) ; e: a Scheme string, association (name . value) or #f.
	   (if entity
	       (set-content-encoding
		entity
		(cond
		 ((string? e) (->jstring e))
		 ((pair? e)   (->jstring (cdr e)))     ; e is (name . value) pair.
		 (else        :java-null-string:)))))) ; Reset to null.

	(make-method!
	 self http/response/type ; Return the Content-Type header (if any).
	 (lambda (self)
	   (cond
	    ((and entity (get-content-type entity)) =>
	     (lambda (h)
	       (if (java-null? h) #f (http/header/unwrap h))))
	    (else #f))))

	(make-method!
	 self http/response/type!
	 (lambda (self e) ; e: a Scheme string, association (name . value) or #f.
	   (if entity
	       (set-content-type
		entity
		(cond
		 ((string? e) (->jstring e))
		 ((pair? e)   (->jstring e (cdr e))) ; e is (name . value) pair.
		 (else        :java-null-string:)))))) ; Reset to null.

	(make-method!
	 self http/response/entity
	 (lambda (self) entity))

	(make-method!
	 self http/response/entity!
	 (lambda (self e)
	   (set! entity (http/entity/new e))
	   (set-entity response entity)))

	(make-method!
	 self http/response
	 (lambda (self) response))

	self))))

(set! http/response/content/length http/response/length)
(set! http/response/content/type   http/response/type)

; (define (response/test/01)
;   (let ((r (object/forge <http/response> 200 "OK by me")))
;     (display (format "response: ~a\n" (http/response r)))
;     (display (format "entity: ~a\n" (http/response/entity r)))
;     (display (format "status: ~a\n" (http/response/status r)))
;     (display (format "reason: ~a\n" (http/response/reason r)))

;     (http/response/header! r "foo" "1111")
;     (http/response/header! r (cons "barz" "2222"))
;     (http/response/header! r "If-Modified" "run-away")
;     r))

; (define (response/test/02)
;   (let ((r (object/forge <http/response> 404 "Not Found moron!" "I didn't even bother looking")))
;     (display (format "response: ~a\n" (http/response r)))
;     (display (format "entity: ~a\n" (http/response/entity r)))
;     (display (format "status: ~a\n" (http/response/status r)))
;     (display (format "reason: ~a\n" (http/response/reason r)))

;     (http/response/header! r "foo" "1111")
;     (http/response/header! r (cons "barz" "2222"))
;     (http/response/header! r "If-Modified" "run-away")
;     r))

