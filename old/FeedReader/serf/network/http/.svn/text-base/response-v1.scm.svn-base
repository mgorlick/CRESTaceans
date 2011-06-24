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

(define <http/response>
  (let ()
    ; Hide the Java grit from view.
    (define-java-class <java-http-response>    |org.apache.http.message.BasicHttpResponse|)
    (define-java-class <java-http-status-line> |org.apache.http.message.BasicStatusLine|)
    (define-generic-java-method get-status-line)   ; StatusLine getStatusLine() for BasicHttpResponse
    (define-generic-java-method set-entity)        ; void setEntity(HttpEntity) for BasicHttpResponse
    (define-generic-java-method set-header)        ; void setHeader(String, String) for BasicHttpResponse
    (define-generic-java-method set-reason-phrase) ; void setReasonPhrase(String) for BasicHttpResponse
    (define-generic-java-method set-status-code)   ; void setStatusCode(int) for BasicHttpResponse
    (define-generic-java-method get-reason-phrase) ; String getReasonPhrase for BasicStatusLine
    (define-generic-java-method get-status-code)   ; int getStatusCode() for BasicStatusLine


    ; Set the headers in a BasicHttpResponse instance o.
    (define (to-java-headers o headers)
      (cond
       ((null? headers) o)
       (else
	(let ((header (car headers)))
	  (set-header o (->jstring (car header)) (->jstring (cdr header))))
	(to-java-headers o (cdr headers)))))

    ; Object constructor.
    (lambda ()
      (let ((self    (make-object))
	    (status  #f)
	    (reason  #f)
	    (headers #f)
					; Content
	    (length   #f)  ; Length in bytes.
	    (type     #f)  ; MIME type.
	    (encoding #f)  ; Usually UTF-8.
	    (entity   #f)  ; For now just a string if anything at all.
	    (java     #f)) ; The original Java object (if any).

	(make-method!
	 self instantiate
	 (lambda (self . arguments)
	   (if (null? arguments)
	       (set! headers '())
	       (instantiate/nonvirgin self (car arguments)))))

	(make-method!
	 self instantiate/nonvirgin
	 (lambda (self java-http-response)
	   (set! java java-http-response)

	   (set! headers (->list (get-all-headers java-http-request)))
	   (translate-headers headers headers)

	   (let ((line (get-status-line java-http-response)))
	     (set! status (->int (get-status-code line)))
	     (set! reason (->string (get-reason-phrase line))))

	   (let* ((e (get-entity java-http-response)))
	     (cond
	      ((not (java-null? e))
	       (set! entity (entity/string/extract e))
	       (let ((n (->number (get-content-length e))))
		 (set! length (and (> n 0) n)))
	       (let ((h (get-content-type e)))
		 (set! type (and (not (java-null? h)) (cdr (http/header/unwrap h)))))
	       (let ((h (get-content-encoding e)))
		 (set! encoding (and (not (java-null? h)) (cdr (http/header/unwrap h))))))
	      (else
	       (set! entity   #f)
	       (set! length   #f)
	       (set! type     #f)
	       (set  encoding #f))))))

	(make-method!
	 self http/response/header!
	 (lambda (self header)
	   (set! headers (cons header headers))))

	(make-method!
	 self http/response/headers
	 (lambda (self)
	   (if java headers (reverse headers))))

	(make-method!
	 self http/response/status
	 (lambda (self) status))

	(make-method!
	 self http/response/reason
	 (lambda (self) reason))

	(make-method!
	 self http/response/length
	 (lambda (self) length))
	
	(make-method!
	 self http/response/type
	 (lambda (self) type))

	(make-method!
	 self http/response/encoding
	 (lambda (self) encoding))

	(make-method!
	 self http/response/entity
	 (lambda (self) entity))

	(make-method!
	 self http/response/entity!
	 (lambda (self e)
	   (set! entity e))) ; For now must be a Scheme string.

	(make-method!
	 self http/response/encoding!
	 (lambda (self e) ; e: Scheme string giving value of Content-Encoding header.
	   (set! encoding e)))

	(make-method!
	 self http/response/type!
	 (lambda (self m) ; m: Scheme string giving MIME type for Content-Type header.
	   (set! type m)))

	(make-method!
	 self http/response/java ; Construct a Java BasicHttpResponse object.
	 (lambda (self o) ; o: BasicHttpResponse instance.
	   (cond
	    ((not java)
	     (set-status-code   o (->jint status))
	     (set-reason-phrase o (->jstring reason))
	     (if type
		 (set! headers (cons '|Content-Type| . type)))
	     (if encoding
		 (set! headers (cons '|Content-Encoding| . encoding)))
	     (to-java-headers o (reverse headers))
	     (if entity
		 (set-entity o (entity/string/new entity)))
	     o)
	    (else #f))))

	self))))
  
