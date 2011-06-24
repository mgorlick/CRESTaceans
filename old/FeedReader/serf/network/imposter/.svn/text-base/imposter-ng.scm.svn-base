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

;; Methods for <serf/imposter>.
(define audit              (make-generic-method))
(define dispatch  (make-generic-method))
(define collect   (make-generic-method))

(define <serf/imposter>
  (let ()
    (define-java-class <java-imposter-wrapper>      |ImposterWrapper|)
    (define-java-class <java-http-request-wrapper>  |HTTPRequestWrapper|)
    (define-java-class <java-http-response-wrapper> |HTTPResponseWrapper|)
    (define-generic-java-field-accessor :request-queue)   ; ImposterWrapper
    (define-generic-java-field-accessor :response-queue)  ; ImposterWrapper
    (define-generic-java-field-accessor :response |resp|) ; HTTPResponseWrapper
    (define-generic-java-field-accessor :context  |obj|)  ; HTTPResponseWrapper
    (define-generic-java-method put) ;  void put(HTTPRequestWrapper) for LinkedBlockingQueue
    (define-generic-java-method take) ; HTTPResponseWrapper take() for LinkedBlockingQueue 
    (define-generic-java-method completed) ; void completed() for HTTPResponseWrapper

    ; request: a Scheme <http/request> object
    ; uri: a Java <java.net.uri> object
    ; context: any Scheme object
    (define (java/request/wrapper request uri context)
      ;(make-request-wrapper :null-java-imposter-wrapper: (http/request request) uri (java-wrap context)))
      (java-new <java-http-request-wrapper> (http/request request) uri (java-wrap context)))

    (define (http/chunked? r)
      (let ((transfer-encoding (http/response/header r "Transfer-Encoding")))
        (and transfer-encoding (string=? "chunked" (cdr transfer-encoding)))))

    ;; Unwrap the HTTPResponseWrapper and deconstruct the response
    ;; w: instance of <java-http-response-wrapper>
    (define (java/response/unwrapper w)
      (let* ((java.http.response (:response w))
	     (r (object/forge <http/response> java.http.response))
	     (context (java-unwrap (:context w)))
	     (content-type (http/response/content/type r))
	     (entity (http/response/entity r))
	     (v (vector
		 (http/response/status r)  ; Status
		 (http/response/reason r)  ; Reason
		 (http/response/headers r) ; Headers
		 #f)))                     ; Payload (contents of entity body if any)

	(if content-type ; There is a Content-Type header so there must be an entity somewhere.
	    (let* ((flavor   (http/header/content/type/parse (cdr content-type))) ; flavor is (media . (p_1 p_2 ...))
		   (media    (and (list? flavor) (car flavor)))
		   (encoding (and (list? flavor) (assoc "encoding" (cdr flavor))))) ; Unused for now.
	      (cond
	       ((string=? media :mime/serf/binary:)
		(let ((buffer (http/entity/buffer entity)))     ; Convert payload of request to Scheme buffer.
		  (vector-set! v 3 (deserialize/from buffer)))) ; Buffer contains a serialized Scheme object.

		((string=? media :mime/json:)
		 (vector-set! v 3 (http/entity/string entity)))

		((or (string=? media :mime/text:) (string=? media :mime/html:))
		 (vector-set! v 3 (http/entity/string entity)))

		((string=? media :mime/form/urlencoded:)
		 (vector-set! v 3 (http/entity/string entity)))

		(else ; Something we don't recognize. Convert it to a string and let the recipient figure it out.
		 (vector-set! v 3 (http/entity/string entity))))))

	(cons v context))) ; Return the deconstructed response and the context.

    (lambda ()
      (let* ((self       (make-object))
	     (user-agent "Serf/Imposter 0.1")
	     (logger     #f)
	     (inbox      #f)
	     (bottom     #f)
	     (counter    1)   ; Counter to match responses with requests in audit log.
	     (dispatcher #f)  ; Thread running the dispatch loop.
	     (collector  #f)) ; Thread running the collect loop.

	(make-method!
	 self instantiate
	 (lambda (self $logger $mailbox)
	   (set! logger $logger)
	   (set! inbox $mailbox)
	   (set! bottom (java-new <java-imposter-wrapper> (->jstring user-agent) $logger))
	   (set! dispatcher
		 (fiber/start
		  (fiber/new
		   (lambda () (dispatch self)))))
	   (set! collector
		 (fiber/start
		  (fiber/new
		   (lambda () (collect self)))))))

					; Logging for imposter.
	(make-method!
	 self audit
	 (lambda (self method message)
	   (log/audit logger (format "serf/imposter.~a: ~a" method message))))

	(make-method!
	 self dispatch
	 (lambda (self)
	   (audit self "dispatch" "start") ; Mark in the log when Imposter started.
	   (let loop ()
	     (match
	      (message/all (? inbox)) ; Read a request message.
	      ;; Issue an HTTP GET request.
	      ;; uri: a <java.net.uri> instance defining the authority, path, and query (if any), for the request
	      ;; tag: a user-generated tag (value) identifying the request (and to distinguish one response from another)
	      ;; reply: the <mailbox> instance to which the HTTP response will be sent
	      (#(/http/get (,uri ,headers ,_entity) ,_metadata ,reply ,echo)
	       (let ((request (object/forge <http/request> "GET" (uri/path+query uri)))
		     (context (vector reply echo counter)))
		 (if headers (http/request/headers! request headers))
		 (audit self "dispatch" (format "request:~d GET ~a ~a"
						counter (uri/authority uri) (uri/path uri)))
		 (put (:request-queue bottom) (java/request/wrapper request uri context)) ; Enqueue the request for the bottom half.
		 (set! counter (+ counter 1))))

	      ;; Issue an HTTP POST request.
	      (#(/http/post (,uri ,headers ,entity) ,_metadata ,reply ,echo)
	       (let* ((request (object/forge <http/request> "POST" (uri/path+query uri)))
		      (context (vector reply echo counter)))
		 (if headers (http/request/headers! request headers))
		 (if entity (http/request/entity! request entity))
		 (audit self "dispatch" (format "request:~d POST ~a ~a"
						counter (uri/authority uri) (uri/path uri)))
		 (put (:request-queue bottom) (java/request/wrapper request uri context)) ; Enqueue the request for the bottom half.
		 (set! counter (+ counter 1))))

	      ((relay ,uri ,reply)
	       (audit self "dispatch" "RELAY not implemented"))

	      ((remote ,uri ,closure ,reply)
	       (audit self "dispatch" "REMOTE not implemented"))

	      ((spawn ,uri ,closure ,reply)
	       (audit self "dispatch" "SPAWN not implemented"))

	      (#(,method ,body ,tag ,reply)
	       (guard (symbol? method))
	       (audit self "dispatch" (format "~a not implemented"  (string-upcase (symbol->string method)))))

	      (,x (audit self "dispatch" "say what?")))

	     (loop))))

	(make-method!
	 self collect
	 (lambda (self)
	   (audit self "collect" "start") ; Mark in the log when Imposter started.
	   (let loop ((wrapper (take (:response-queue bottom))))   ; Wait for the next response to appear from the bottom.
	     (let* ((extraction (java/response/unwrapper wrapper)) ; Extraction is (#(status reason headers payload) . context)
		    (deconstruction (car extraction)) ; Deconstruction of HTTP response as #(status reason headers payload)
		    (context (cdr extraction))        ; Context is #(origin echo counter).
		    (origin  (vector-ref context 0))
		    (echo    (vector-ref context 1))
		    (counter (vector-ref context 2)))
	       (completed wrapper) ; Let the Java bottom know that we are done with the wrapper.
	       (audit self "collect" (format "response:~d status:~a reason:~a"
					     counter (vector-ref deconstruction 0) (vector-ref deconstruction 1)))
	       (! origin deconstruction :no-metadata: :no-reply: echo)) ; Send the deconstructed response back to its rightful origin.
	     (loop (take (:response-queue bottom))))))

	self))))

	
