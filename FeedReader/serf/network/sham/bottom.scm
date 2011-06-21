;; Bridge to Apache NIO (Nonblocking I/O) HttpServer core.

(define-java-class <java-nio-http-server> |NioHttpServer|)
(define-generic-java-method go) ; void go() in <java-nio-http-server>

(define (bottom/new port request-responder connection-listener document-root)
  (let* ((bottom
	  (java-new
	   <java-nio-http-server>
	   (->jint port)
	   (if (and document-root (string? document-root)) (->jstring document-root) :java-null-string:)
	   request-responder
	   connection-listener))

	 (bottom/fiber
	  (fiber/new
	   (lambda ()
	     (with/fc
	      (lambda (m e) (print-exception (make-exception m e) #t)) ; Print the exception plus a full stack trace.
	      (lambda () (go bottom)))))))

    (fiber/start bottom/fiber) ; Start the NIO HTTP server as a separate thread.
    bottom)) ; Return the NIO HTPP server instance.

