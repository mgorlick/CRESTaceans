;; Bridge to Apache NIO (Nonblocking I/O) HTTPClient core.
(define-java-class <java-nio-http-client> |NioHttpClient|)
(define :null-java-nio-http-client:)
(define-generic-java-method go) ; void go() in <java-nio-http-client>
(define-generic-java-method uri-for-request) ; String uriForRequest(java.net.URI) in <java-nio-http-client>

;; Construct and launch a Nonblocking I/O HTTP client returning the client object as a value.
;; Note: This could be elaborated by allowing other threads to establish CREST links for notification
;; of HTTP client events (like termination or unexpected interrupt).

;; user-agent: string identifying the user agent, for example, "CREST Imposter/1.0"
;; request-handler: proxy implementing the HttpRequestExecutionHandler interface
;; connection-listener: proxy implementing a ConnectionEventLogger.
(define (bottom/new user-agent request-responder connection-listener)
  (let* ((bottom
	  (java-new <java-nio-http-client>
		    (->jstring user-agent) request-responder connection-listener))
	 (bottom/fiber
	  (fiber/new
	   (lambda ()
	     (with/fc
	      (lambda (m e) (print-exception (make-exception m e) #t)) ; Print the exception plus a full stack trace.
	      (lambda () (go bottom)))))))
    (fiber/start bottom/fiber) ; Start the NIO HTTP client as a separate thread.
    bottom)) ; Return the NIO HTPP client instance so that we can use it for session establishment in the future.

;; Returns a <jstring> that is the path + query (if any) of <java.net.uri> u.
;; A cheesy hack for efficiency since Imposter and Sham encode serialized Scheme values as an URI query.
;; This method has no business being the in the nio-http-client. I was in a hurry.
(define (uri-to-request-path u)
  (uri-for-request :null-java-nio-http-client: u))

(set! :null-java-nio-http-client: (java-null <java-nio-http-client>))


