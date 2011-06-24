;; Bridge to Apache NIO (Nonblocking I/O) HTTPClient core.
(import s2j)

(define-java-class <java-nio-http-client> |NIO_HTTP_Client|)
(define-generic-java-methods
  set-target-host
  go
  open-session)

;; Construct and launch an NIO HTTP client returning the client object as a value.
;; Note: This could be elaborated by allowing other threads to establish CREST links for notification
;; of HTTP client events (like termination or unexpected interrupt).
;; user-agent: string identifying the user agent, for example, "CREST Imposter/1.0"
;; request-handler: SISC proxy implementing the HttpRequestExecutionHandler interface
;; connection-listener: SISC proxy implementing a ConnectionEventLogger.
(define (make-http-client user-agent request-responder connection-listener)
  (let* ((client
	  (java-new <java-nio-http-client>
		    (->jstring user-agent) request-responder connection-listener))
	 (imposter 
	  (make <fiber>
		(lambda ()
		  (with/fc
		   (lambda (m e) (print-exception (make-exception m e) #t)) ; Print the exception plus a full stack trace.
		   (lambda () (go client)))))))
    (start imposter) ; Run the NIO HTTP client as a separate thread.
    client))         ; Return the HTPP client instance so that we can use it for session establishment in the future.

;; Open an HTTP session to a host using the given NIO HTTP client.
;; nio-http-client: an NIO HTTP client object
;; host: string giving the IP address ("192.168.3.27") or DNS name ("www.example.com") of an HTTP/1.1 web server
;; port: integer port number for web server
;; session-handler: SISC proxy implementing the SessionRequestCallback interface
;(define (open-http-session nio-http-client host port session-handler)
;  (open-session nio-http-client (->jstring host) (->jint port) session-handler))

(define-java-class <java-http-context> |org.apache.http.protocol.SyncBasicHttpContext|)
(define-generic-java-methods
  get-attribute set-attribute remove-attribute)

;; A <java-http-host> is an endpoint of an HTTP connection.
(define-java-class <java-http-host> |org.apache.http.HttpHost|)
(define-generic-java-methods
  get-host-name    ; Returns, as a Java string, the DNS host name (www.google.com) or the IP address (66.102.7.99).
  get-port         ; Returns, as a Java integer, the port number of the host.
  (to-uri |toURI|) ; Returns, as a Java string, the URI of the host (http://www.google.com:80 or http://66.102.7.99:80).
  to-host-string)  ; Returns, as a Java string, the endpoint address (www.google.com:80 or 66.102.7.99:80).
                   ; to-string: Returns, as a Java string, the URI of the endpoint (http://www.google.com:80 or http://66.102.7.99:80).

(define HTTP-CONNECTION   (->jstring "http.connection"))
(define HTTP-PROXY-HOST   (->jstring "http.proxy_host"))
(define HTTP-REQUEST-SENT (->jstring "http.request_sent"))
(define HTTP-REQUEST      (->jstring "http.request"))
(define HTTP-RESPONSE     (->jstring "http.response"))
(define HTTP-TARGET-HOST  (->jstring "http.target_host"))

(define TRANSACTION-ID     (->jstring "transaction-id"))
(define HTTP-GET-METHOD    (->jstring "GET"))
(define HTTP-PUT-METHOD    (->jstring "PUT"))
(define HTTP-POST-METHOD   (->jstring "POST"))
(define HTTP-DELETE-METHOD (->jstring "DELETE"))

(define CREST-IMPOSTER-HTTP-CONTEXT #f)

;; Define a SISC proxy for the HttpRequestExecutionHandler interface.
(define-java-class
  <java-http-request-responder> |org.apache.http.nio.protocol.HttpRequestExecutionHandler|)
(define-generic-java-methods
  initalize-context ; The morons who defined this interface can't spell.
  finalize-context
  submit-request
  handle-response)

;; Remove the package name from the name of an instance.
;; For example,  org.apache.http.protocol.SyncBasicHttpContext@afe17b => SyncBasicHttpContext@afe17b .
(define (snip-package-name s)
  (let ((i (string-index-right s #\.)))
    (if i (substring s (+ i 1) (string-length s)) s)))

(define (audit-http-request-responder logger method http-context message)
  (serf-audit
   logger
   (string-append
    "proxy-http-request-responder." method ": "
    (http-context-to-authority http-context) (if message " ")
    (or message ""))))

;; Extracts the target host out of the HTTP context and returns it as a Scheme string "host:port".
(define (http-context-to-authority-string http-context)
  (let ((target-host (get-attribute http-context HTTP-TARGET-HOST)))
    (->string (to-host-string target-host)))) ; Return target host as "www.example.com:8080" or "198.12.201.3:8080".

(define (http-context-to-authority http-context)
  (let ((target-host (get-attribute http-context HTTP-TARGET-HOST)))
    (make-authority (->string (get-host-name target-host)) (->int (get-port target-host)))))

(define HTTP-REQUESTS-SENT (->jstring "http-requests-sent"))


;; Keys are uuid strings and values are <mailbox> instances.
;; The table is thread-safe with strong references for keys.
(define CREST/TRANSACTIONS
  (make-hash-table string-ci=? hash-by-string-ci=? #t #f))

;; logger: Logger instance for this request responder.

;; outgoing: A hash table for managing all outgoing requests for the host authority. It comprises target/mailbox pairs where:
;;   target: the authority, as a string host:port, of a target host
;;   mailbox: a <mailbox> instance containing work orders (http-request mailbox) destined for the given target authority
;;     http-request: an HTTP request with one of the following methods: GET, PUT, POST, DELETE, SPAWN, REMOTE, or RELAY
;;     mailbox: a CREST mailbox to which the response will be posted
;; outgoing is a global hash table as there is exactly one instance in a CREST peer that records 
;; all outstanding outgoing requests.

;; transactions: A hash table of transaction-id/mailbox pairs where a unique transaction-id is assigned to each outgoing
;;   SPAWN or REMOTE request and mailbox is the mailbox to which the response must be directed.
;;   This hash table is global---there is exactly one instance in a CREST peer, shared among all
;;   SPAWN and REMOTE requests. No transactions are necessary for RELAY requests as they are pure
;;   message delivery where no response is expected or required.
(define-java-proxy
  (proxy-http-request-responder logger outgoing transactions) ; Proxy constructor.
  (<java-http-request-responder>) ; Interfaces implemented by this proxy.

  ; http-context: SyncBasicHttpContext
  ; attachment: HttpHost
  (define (initalize-context this http-context attachment)

    ; set-target-host is a workaround since the set-attribute method of http-context is not resolved properly.
    ; Note: Check the implementation of method resolution in util/s2j.scm to resolve this problem.
    (set-target-host (java-null <java-nio-http-client>) http-context HTTP-TARGET-HOST attachment)

    (set-attribute http-context HTTP-REQUESTS-SENT (java-wrap (make <queue>)))
    (audit-http-request-responder logger "initalize-context" http-context) #f)

  (define (finalize-context this http-context)
    (audit-http-request-responder logger "finalize-context" http-context))

  ; Called whenever connection is to ready to accept another request.
  (define (submit-request this http-context)
    (let* ((http-host (get-attribute http-context HTTP-TARGET-HOST))
	   (authority (->string (to-host-string http-host)))
	   (ticket (next-outgoing-ticket authority outgoing))
	   (method (and ticket (http-method-of (:request ticket)))))
      (audit-http-request-responder logger "submit-request" http-context)
      (if method
	  (case method
	    (("GET")
	     (let ((sent (java-unwrap (get-attribute http-context HTTP-REQUESTS-SENT))))
	       (:timestamp! ticket (now))
	       (put! sent ticket)
	       (:request ticket)))

	    (("PUT" "POST")
	     (audit-http-request-responder logger "submit-request" http-context "PUT/POST not implemented")
	     NULL-HTTP-REQUEST)
	    
	    (("DELETE")
	     (audit-http-request-responder logger "submit-request" http-context "DELETE not implemented")
	     NULL-HTTP-REQUEST)

	    (("REMOTE" "SPAWN" "RELAY")
	     (audit-http-request-responder logger "submit-request" http-context "REMOTE/SPAWN/RELAY not implemented")
	     NULL-HTTP-REQUEST)

	    (else
	     (audit-http-request-responder logger "submit-request" http-context (string-append "unknown method <" method ">"))
	     NULL-HTTP-REQUEST))))))

  ; Called whenever a response arrives.
  ; response: <java-http-response>
  ; http-context: <java-http-context>
  (define (handle-response this response http-context)
    (let* ((status-line (get-status-line response)) ; <java-status-line>
	   (status-code (->int (get-status-code status-line)))
	   (reason-phrase (->string (get-reason-phrase status-line)))
	   (entity (get-entity response))
	   (ticket (take! (java-unwrap (get-attribute http-context HTTP-REQUESTS-SENT)) #f)))

      (cond
       (ticket ; Dispatch the response to the waiting mailbox.
	(audit-http-request-responder
	 logger "handle-response" http-context
	 (string-append (number->string status-code) " " (:uuid ticket)))
	(! (:mailbox ticket)
	   (list
	    'http-response
	    (:uuid ticket)
	    (cons status-code reason-phrase) ; (status . reason)
	    (http-entity-to-string entity))))

       (else ; Internal error. No corresponding ticket for this response.
	(audit-http-request-responder
	 logger "handle-response" http-context
	 (string-append (number->string status-code) " error: no corresponding ticket"))))))

  (define (to-string this) (->jstring "http-request-responder")))

(define-java-class <java-session-request> |org.apache.http.impl.nio.reactor.SessionRequestImpl|)
(define-generic-java-methods
  get-local-address  ; Returns InetSocketAddress.
  get-remote-address ; Returns InetSocketAddress.
  get-exception)     ; Returns IOException

(define-java-class <java-internet-socket-address> |java.net.InetSocketAddress|)
(define-generic-java-methods
  get-host-name  ; Returns String.
  get-port)      ; Returns int.




(define-java-class <java-connection-listener> |org.apache.http.nio.protocol.EventListener|)
(define-generic-java-methods
  connection-open
  connection-closed
  connection-timeout
  |fatal-IO-exception|
  fatal-protocol-exception)
(define-java-class
  <java-default-nio-http-client-connection> |org.apache.http.impl.nio.DefaultNHttpClientConnection|)
(define-java-class <java-http-exception> |org.apache.http.HttpException|)
(define-java-class <java-io-exception> |java.io.IOException|)
(define-generic-java-methods get-message)

;;(define CREST-IMPOSTER-CONNECTION #f)

;; logger: Serf logger instance
;; method: method name as string
;; c: <java-default-nio-http-connection> instance
(define (audit-connection logger method c)
  (serf-audit
   logger
   (string-append "proxy-connection-listener." method ": " (->string (to-string c)))))

;; logger: Serf logger instance
;; method: method name as string
;; e: <java-http-exception> or <java-io-exception> instance
;; c: <java-default-nio-http-connection> instance
(define (audit-connection-exception logger method e c)
  (serf-audit
   logger
   (string-append "proxy-connection-listener." method ": " (->string (to-string c)) (->string (to-string e)))))

;; For each method:
;; c: <java-default-nio-http-client-connection> instance
;; e: either a <java-http-exception> or a <java-io-exception> instance
(define-java-proxy
  (proxy-connection-listener logger)
  (<java-connection-listener>)

  (define (connection-open this c)
    (audit-connection logger "connection-open" c))

  (define (connection-timeout this c)
    (audit-connection logger "connection-timeout" c))

  (define (connection-closed this c)
    (audit-connection logger "connection-closed" c))

  (define (|fatal-IO-exception| this e c)
    (audit-connection-exception logger "fatal-IO-exception" e c))

  (define (fatal-protocol-exception this e c)
    (audit-connection-exception logger "fatal-protocol-exception" e c)))


;; Tests

(define (imposter-test01)
  (display (request-execution-handler #f #f))
  (newline))

(define CREST-IMPOSTER #f)
(define CREST-IMPOSTER-REQUEST-HANDLER #f)
(define CREST-IMPOSTER-CONNECTION-LISTENER #f)
(define CREST-IMPOSTER-SESSION-HANDLER #f)

;; Open a connection to www.google.com:80. using the raw IPv4 address.
(define (imposter-test98)
  (let* ((logger (this-logger))
	 (user-agent "CREST/Imposter 1.0")
	 (request-responder (proxy-http-request-responder logger (make <queue>)))
	 (connection-listener (proxy-connection-listener logger))
	 (http-client (make-http-client user-agent request-responder connection-listener))
	 (session-responder (proxy-http-session-responder logger)))
    (set! CREST-IMPOSTER http-client)
    (open-http-session http-client "66.102.7.99" 80 session-responder)))

;; Open a connection to www.google.com:80.
(define (imposter-test99)
  (let* ((logger (this-logger))
	 (user-agent "CREST/Imposter 1.0")
	 (request-responder (proxy-http-request-responder logger (make <queue>)))
	 (connection-listener (proxy-connection-listener logger))
	 (http-client (make-http-client user-agent request-responder connection-listener))
	 (session-responder (proxy-http-session-responder logger)))
    ;(set! CREST-IMPOSTER-REQUEST-HANDLER request-handler)
    ;(set! CREST-IMPOSTER-CONNECTION-LISTENER connection-listener)
    ;(set! CREST-IMPOSTER-SESSION-HANDLER session-handler)
    (set! CREST-IMPOSTER http-client)
    (open-http-session http-client "www.google.com" 80 session-responder)))
