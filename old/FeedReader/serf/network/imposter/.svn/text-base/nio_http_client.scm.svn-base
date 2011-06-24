;; Bridge to Apache NIO (Nonblocking I/O) HTTPClient core.
(import s2j)

;; String library.
(require-library 'sisc/libs/srfi/srfi-13)
(import srfi-13)

(define-java-class <java-nio-http-client> |NIO_HTTP_Client|)
(define-generic-java-methods
  set-target-host
  go
  open-session
  uri-for-request)

(define :null-java-http-client: (java-null <java-nio-http-client>))

;; Construct and launch an NIO HTTP client returning the client object as a value.
;; Note: This could be elaborated by allowing other threads to establish CREST links for notification
;; of HTTP client events (like termination or unexpected interrupt).
;; user-agent: string identifying the user agent, for example, "CREST Imposter/1.0"
;; request-handler: SISC proxy implementing the HttpRequestExecutionHandler interface
;; connection-listener: SISC proxy implementing a ConnectionEventLogger.
(define (make-nio-http-client user-agent request-responder connection-listener)
  (let* ((nio-http-client
	  (java-new <java-nio-http-client>
		    (->jstring user-agent) request-responder connection-listener))
	 (nio-fiber
	  (make <fiber>
		(lambda ()
		  (with/fc
		   (lambda (m e) (print-exception (make-exception m e) #t)) ; Print the exception plus a full stack trace.
		   (lambda () (go nio-http-client)))))))
    (start nio-fiber) ; Run the NIO HTTP client as a separate thread.
    nio-http-client)) ; Return the NIO HTPP client instance so that we can use it for session establishment in the future.

;; Open an HTTP session to a host using the given NIO HTTP client.
;; nio-http-client: an NIO HTTP client object
;; host: string giving the IP address ("192.168.3.27") or DNS name ("www.example.com") of an HTTP/1.1 web server
;; port: integer port number for web server
;; session-handler: SISC proxy implementing the SessionRequestCallback interface
;(define (open-http-session nio-http-client host port session-handler)
;  (open-session nio-http-client (->jstring host) (->jint port) session-handler))

(define-java-class <java-http-context> |org.apache.http.protocol.SyncBasicHttpContext|)
(define-generic-java-methods
  get-attribute set-attribute remove-attribute to-string)

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

(define (audit-http-request-responder logger method context)
  (let ((authority (context-to-authority context))
	(id (java-object-to-id context)))
  (serf-audit
   logger
   (string-append "proxy-http-request-responder." method ": " authority " context:" id))))


; (define (audit-http-request-responder logger method context message)
;   (serf-audit
;    logger
;    (if message
;        (string-append "proxy-http-request-responder." method ": " (to-authority context) " " message)
;        (string-append "proxy-http-request-responder." method ": " (to-authority context)))))



;(define-generic to-authority)

;; Extracts the target host out of the HTTP context and returns it as a Scheme string "host:port".
; (define-method (to-authority (<java-http-context> context))
;   (let ((target-host (get-attribute context HTTP-TARGET-HOST)))
;     (->string (to-host-string target-host)))) ; Return target host as "www.example.com:8080" or "198.12.201.3:8080".

; (define (http-context-to-authority http-context)
;   (let ((target-host (get-attribute http-context HTTP-TARGET-HOST)))
;     (make-authority (->string (get-host-name target-host)) (->int (get-port target-host)))))

;(define HTTP-REQUESTS-SENT (->jstring "http-requests-sent"))


;; Keys are uuid strings and values are <mailbox> instances.
;; The table is thread-safe with strong references for keys.
;(define CREST/TRANSACTIONS
;  (make-hash-table string-ci=? hash-by-string-ci=? #t #f))

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

(define :imposter.authority: (->jstring "imposter.authority"))
(define :imposter.pending:   (->jstring "imposter.pending"))
;(define :imposter.context-id: (->jstring "imposter.context-id"))
(define-java-proxy
  (proxy-http-request-responder imposter) ; Proxy constructor.
  (<java-http-request-responder>) ; Interfaces implemented by this proxy.

  ; context: <java-http-context>
  ; attachment: HttpHost
  (define (initalize-context this context attachment)
    (let* ((host (get-host-name attachment))
	   (port (get-port attachment))
	   (address (java-new <java-internet-socket-address> host port))
	   (canonical (internet-socket-address-to-authority address))) ; For example, "www.foo.org/63.109.18.27:8080".
      (set-attribute context :imposter.authority: (java-wrap canonical))
      ;(guarantee-pending imposter context)
      (set-attribute context :imposter.pending: (java-wrap (make <queue>)))
      (audit-http-request-responder (:logger imposter) "initalize-context" context)))
    ; set-target-host is a workaround since the set-attribute method of context is not resolved properly.
    ; Note: Check the implementation of method resolution in util/s2j.scm to resolve this problem.
    ;(set-target-host :null-java-http-client: context HTTP-TARGET-HOST attachment)

    ; Each connection (as represented by an context) has a queue of pending requests.
    ; A pending request is one which has been sent but for which no response has been received.
    ; Pending requests are queued in order of transmission.
    ;(guarantee-pending imposter context)
    ;(set-attribute context HTTP-REQUESTS-SENT (java-wrap (make <queue>)))

  ; This should be reimplemented to simply call an Imposter method as do submit-request and handle-response.
  ; context: <java-http-context>
  (define (finalize-context this context)
    (let ((authority (context-to-authority context))
	  (queue (java-unwrap (get-attribute context :imposter.pending:))))
      ; We can not remove the :imposter.authority: attribute as their is a race as to which will be called first,
      ; finalize-context in http-request-responder or connection-close in the connection-listener, as connection-close
      ; requires the :imposter.authority: attribute for proper logging.
      (remove-attribute context :imposter.pending:)
      (serf-audit
       (:logger imposter) 
       ; NOTE: (%backlog queue) here is a hack as the method (backlog queue) is unresolved here for reasons I do not understand.
       (string-append "http-request-responder.finalize-context: " authority " pending:" (number->string (%backlog queue))))))

  ; Called whenever connection is to ready to accept another request.
  ; context: <java-http-context>
  (define (submit-request this context)
    (submit-request imposter context))

  ; Called whenever a response arrives.
  ; response: <java-http-response>
  ; context: <java-http-context>
  (define (handle-response this response context)
    (handle-response imposter response context))

  (define (to-string this) (->jstring "http-request-responder")))

; Extract the authority (a <string> DNS/IP:port) from <java-http-context> c.
(define (context-to-authority c)
  (java-unwrap (get-attribute c :imposter.authority:)))
; Extract the uuid assigned to a context.
(define (context-uuid c)
  (java-unwrap (get-attribute c :imposter.context-id:)))


(define-java-class <java-session-request> |org.apache.http.impl.nio.reactor.SessionRequestImpl|)
(define-generic-java-methods
  get-local-address  ; Returns InetSocketAddress.
  get-remote-address ; Returns InetSocketAddress.
  get-exception)     ; Returns IOException

(define-java-class <java-internet-socket-address> |java.net.InetSocketAddress|)
(define-generic-java-methods
  get-address    ; Returns <java-internet-address>.
  get-host-name  ; Returns DNS name as String.
  get-port)      ; Returns int.

(define-java-class <java-internet-address> |java.net.InetAddress|)
(define-generic-java-methods
  get-host-address ; Returns IP dotted address as String.
  get-host-name)   ; Returns DNS name as String.

;; Given a <java-internet-socket-address> a returns a Scheme string
;; DNS/IP:port where DNS is the DNS name of the remote endpoint,
;; IP is the dotted IP address of the remote endpoint, and
;; port is the port number. If reverse lookup failed then DNS is the empty string
;; and the result has the form /IP:port.
;; For example, "www.google.com/74.125.19.106:80"
(define (internet-socket-address-to-authority a) (java-to-string a))

(define-java-class <java-connection-listener> |org.apache.http.nio.protocol.EventListener|)
(define-generic-java-methods
  connection-open
  connection-closed
  connection-timeout
  |fatal-IO-exception|
  fatal-protocol-exception)

(define :client-connection-active:  (->jint 0))
(define :client-connection-closing: (->jint 1))
(define :client-connection-closed:  (->jint 2))

(define-java-class
  <java-default-nio-http-client-connection> |org.apache.http.impl.nio.DefaultNHttpClientConnection|)
(define-generic-java-methods
  get-context        ; Returns the <java-http-context> affiliated with this connection.
  get-remote-address ; Returns <java-internet-address> of remote endpoint.
  get-remote-port    ; Returns port number (Java int) of remote endpoint.
  get-status         ; Returns the status of this connection as an int: active [0], closing [1], or closed [2]
  hash-code
  request-output     ; Requests event notifications to be triggered when the underlying channel is ready for output operations.
  is-open)           ; Returns true if connection is open and false otherwise.

; Returns #t if connection c is open and #f it is closed.
(define (connection-alive? c) (eqv? (get-status c) :client-connection-active:))

;; Cheap mechanism to extract authority from an NIO HTTP client connection.
;; c: <java-default-nio-http-client-connection>
(define (connection-to-authority c)
  (let ((s (java-to-string c))) ; String representation of c is "[www.google.com/67.28.55.33:80]"
    (substring/shared s 1 (- (string-length s) 1))))

(define-java-class <java-http-exception> |org.apache.http.HttpException|)
(define-java-class <java-io-exception> |java.io.IOException|)
(define-generic-java-methods
  get-message)


;; Returns a string (in hex) that is (more or less) a unique id for a Java object x.
(define (java-object-to-id x) (number->string (->number (hash-code x)) 16))

;;(define CREST-IMPOSTER-CONNECTION #f)

;; logger: Serf logger instance
;; method: method name as string
;; c: <java-default-nio-http-connection> instance
(define (audit-connection logger method context)
  (let* ((authority (context-to-authority context))
	 (id  (java-object-to-id context))) ; Hex id to distinguish among multiple connections for the same authority.
    (serf-audit logger (string-append "proxy-connection-listener." method ": " authority " context:" id))))

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
  (proxy-connection-listener imposter)
  (<java-connection-listener>)

  (define (connection-open this c)
    (let* ((context (get-context c))
	   (authority (context-to-authority context)))
      (hashtable/put! (:connections imposter) authority c)
      (audit-connection (:logger imposter) "connection-open" context)))

  (define (connection-timeout this c)
    (let* ((context (get-context c))
	   (authority (context-to-authority context)))
      (hashtable/remove! (:connections imposter) authority)
      (audit-connection (:logger imposter) "connection-timeout" context)))

  (define (connection-closed this c)
    (let* ((context (get-context c))
	   (authority (context-to-authority context)))
      (hashtable/remove! (:connections imposter) authority)
      (audit-connection (:logger imposter) "connection-closed" context)))

  (define (|fatal-IO-exception| this e c)
    ; Not sure what remedial action is appropriate here.
    (audit-connection-exception (:logger imposter) "fatal-IO-exception" e c))

  (define (fatal-protocol-exception this e c)
    ; Not sure what remedial action is appropriate here.
    (audit-connection-exception (:logger imposter) "fatal-protocol-exception" e c))

  (define (to-string this) (->jstring "connection-listener")))

;; Returns a <jstring> that is the path + query (if any) of <java.net.uri> u.
;; A cheesy hack for efficiency since Imposter and Sham encode serialized Scheme values as an URI query.
(define (uri-to-request-path u)
  (uri-for-request :null-java-http-client: u))