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

(define-record-type <ticket>
  (make-ticket authority uri request tag reply id)
  <ticket>?
  (authority :ticket-authority)           ; Target authority as string DNS/IP:port, for example www.example.com/69.54.221.17:8080
  (uri       :ticket-uri)
  (request   :ticket-request)             ; Serf <http/request> instance.
  (tag       :ticket-tag)                 ; Object assigned by requester to distinguish this request from others.
  (reply     :ticket-reply)               ; <mailbox> instance to which response is delivered.
  (id        :ticket-id)                  ; Unique integer assigned by HTTP client.
  (sent      :ticket-sent :ticket-sent!)) ; Timestamp when request transmitted by HTTP client.


;(define-java-class <java-nio-http-client-connection> |org.apache.http.nio.NHttpClientConnection|)
(define-generic-java-method get-context)      ; HttpContext getContext(). Extract the context of a connection.
(define-generic-java-method open?)            ; boolean isOpen(). Returns true iff the connection is open.
(define-generic-java-method request-output)   ; void requestOutput(). Kick the connection.
(define-generic-java-method open-session)     ; To open session from java-bottom below.
;(define-generic-java-method to-string)        ; Obtain String representation of a Java object.
(define-generic-java-method get-attribute)    ; Object getAttribute(String). Get an attribute from an HttpContext.
(define-generic-java-method set-attribute)    ; Object setAttribute(String, Object). Set an attribute in an HttpContext.
(define-generic-java-method remove-attribute) ; Object removeAttribute(String). Remove an attribute from an HttpContext.
(define-generic-java-method get-host)         ; String getHost() for java.net.URI
(define-generic-java-method get-port)         ; int getPort() ;for java.net.URI

;; Methods for <serf/imposter>.
(define audit               (make-generic-method))
(define bump                (make-generic-method))
(define imposter/dispatch   (make-generic-method))
(define next-ticket         (make-generic-method))
(define schedule-connection (make-generic-method))
(define schedule-ticket     (make-generic-method))
;; ALL other generic methods are supplied by module serf/proxy.

(define (<serf/imposter>)
  (let* ((self                (make-object))
	 (user-agent          "Serf/Imposter 0.1")
	 (logger              #f)
	 (outgoing            (make-hashtable string=? hash-by-string= #t #f)) ; Thread-safe, strong key references.
	 (connections         (make-hashtable string=? hash-by-string= #t #f)) ; Thread-safe, strong key references.
	 (inbox               #f)
	 (id-counter          1)
	 (request-responder   (proxy-http-request-responder self))
	 (connection-listener (proxy-connection-listener self))
	 (session-responder   (proxy-http-session-responder self))
	 (java-bottom         (bottom/new user-agent request-responder connection-listener)))

    (define (set-headers headers request)
      (let loop ((all headers))
	(cond
	 ((null? all) request)
	 (else
	  (http/request/header! (car all) request)
	  (loop (cdr headers) request)))))

    ; Initialization post-construction.
    (make-method!
     self instantiate
     (lambda (self $logger $mailbox)
       (set! logger $logger)
       (set! inbox $mailbox)))

    ; Pick up the next waiting ticket for the authority.
    ; Returns the ticket (if any) or #f.
    (make-method!
     self next-ticket
     (lambda (self authority)
       (let ((q (hashtable/get outgoing authority)))
	 (and q (queue/take! q #f)))))

    ; Generate a new id for a ticket.
    (make-method!
     self bump
     (lambda (self)
       (let ((n id-counter))
	 (set! id-counter (+ id-counter 1))
	 n)))

    ; Logging for imposter.
    (make-method!
     self audit
     (lambda (self method message)
       (log/audit logger (format "serf/imposter.~a: ~a" method message))))

    (make-method!
     self imposter/dispatch
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
	   (let* ((authority (address/canonical (get-host uri) (get-port uri)))
		  (request
		   (object/forge <http/request> "GET" (uri-to-request-path uri)))
		  (ticket    (make-ticket authority uri request echo reply (bump self))))
	     (if headers (http/request/headers! request headers))
	     (schedule-ticket self ticket)
	     (schedule-connection self authority uri)
	     (audit self "dispatch" (format "GET ~a ~a" authority (uri/path uri)))))

	  ;; Issue an HTTP POST request.
	  (#(/http/post (,uri ,headers ,entity) ,_metadata ,reply ,echo)
	   (let* ((authority (address/canonical (get-host uri) (get-port uri)))
		  (request
		   (object/forge <http/request> "POST" (uri-to-request-path uri)))
		  (ticket    (make-ticket authority uri request echo reply (bump self))))
	     (if headers (http/request/headers! request headers))
	     (if entity (http/request/entity! request entity))
	     (schedule-ticket self ticket)
	     (schedule-connection self authority uri)
	     (audit self "dispatch" (format "POST ~a ~a" authority (uri/path uri)))))

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

     ; Ensures canonical authority, DNS/IP:port (as string), is installed as key in :outgoing table and
     ; enqueues ticket for transmission to authority.
     (make-method!
      self schedule-ticket
      (lambda (self ticket)
	(let ((authority (:ticket-authority ticket)))
          ; Ensure authority/queue pair appears in :outgoing table and enqueue ticket.
          ; Note that queue is thread-safe since NIO threads will be accessing it simultaneously.
	  (queue/put!
	   (hashtable/get! outgoing authority (lambda () (queue/new #t)) #f) ; #f indicates that thunk is safe.
	   ticket)
	  (audit self "schedule-ticket" (format "ticket-id:~d" (:ticket-id ticket))))))

     (make-method!
      self schedule-connection
      (lambda (self authority uri)
	(let ((c (hashtable/get connections authority)))
	  (cond
	   ((null? c)) ; Connection (re)establishment is already underway so do nothing.

	   ((and (java-object? c) (->boolean (open? c)))
	    (request-output c) ; Kick the connection to notify us (via connection-listener) when ready for feeding.
	    (audit self "schedule-connection" (format "~a kicked" authority)))

	   (else ; Either there is no connection at all or the connection was closed or timed out so (re)open it.
	    (hashtable/put! connections authority '()) ; The empty list is a placeholder in connections table.
            ; Contact the bottom NIO layer and attempt to establish a session for the authority.
	    (let ((session (open-session java-bottom (get-host uri) (get-port uri) session-responder)))
	      (audit
	       self "schedule-connection"
	       (format "~a session:~x" authority (java-object-to-id session)))))))))

     (make-method! ; HTTP request responder.
      self initialize-context
      (lambda (self context host port) ; <java-http-context> <java-string> <jint>
	  (audit self "initialize-context" (format "starting ~a ~a ~a" context host port))
	(let ((authority (address/canonical host port)))
	  (audit self "initialize-context" (format "authority ~a" authority))
	  (set-attribute context :imposter.authority: (->jstring authority))
          ; Queue for tickets awaiting response on this connection/context.
	  (audit self "initialize-context" (format "authority set"))
	  (set-attribute context :imposter.pending: (java-wrap (queue/new #f)))
	  (audit self "initialize-context" (format "~a context:~x" authority (java-object-to-id context))))))

     ;; HTTP request responder.
     (make-method!
      self finalize-context
      (lambda (self context) ; <java-http-context>
	(let ((authority (->string (get-attribute    context :imposter.authority:)))
	      (pending   (java-unwrap (remove-attribute context :imposter.pending:))))
          ; We can not remove the :imposter.authority: attribute from the context as their is a race as to which will be called first,
          ; finalize-context in http-request-responder or connection-close in the connection-listener, as connection-close
          ; requires the :imposter.authority: attribute for proper logging.
	  (audit
	   self "finalize-context"
	   (format "~a context:~x pending:~d" authority (java-object-to-id context) (queue/occupancy pending))))))

     (make-method! ;; HTTP request responder.
      self submit-request
      ; If a <java-http-request> request r is waiting for to be sent to the authority represented
      ; by the given context then return r. If no such request is waiting then return the null <java-http-request>.
      (lambda (self context) ; <java-http-context>
	(let* ((authority (->string (get-attribute context :imposter.authority:)))
	       (ticket    (next-ticket self authority)))
	  (cond
	   (ticket
	    (audit
	     self "submit-request"
	     (format "~a context:~x ticket-id:~d" authority (java-object-to-id context) (:ticket-id ticket)))
	    (queue/put! (java-unwrap (get-attribute context :imposter.pending:)) ticket)
	    (:ticket-sent! ticket (now/utc))
	    (schedule-connection self authority (:ticket-uri ticket)) ; Just in case.
	    (http/request (:ticket-request ticket))) ; Extract Java HTTP request instance from Serf <http/request> instance.
	   (else
	    (audit
	     self "submit-request"
	     (format "~a context:~x no ticket" authority (java-object-to-id context)))
	    :null-java-http-request:))))) ; No waiting outgoing requests for the authority represented by this context.

     (make-method!
      self handle-response ; HTTP request responder.
      ; We have a response to an HTTP request.
      ; Send the response off to the mailbox specified in the ticket.
      (lambda (self response context) ; <java-http-response> and <java-http-context> respectively.
	(let* ((authority (->string (get-attribute context :imposter.authority:)))
	       (pending   (java-unwrap (get-attribute context :imposter.pending:)))
	       (ticket    (queue/take! pending #f)))
	  (cond
	   (ticket
	    (let ((to   (:ticket-reply ticket))
		  (body (object/forge <http/response> response)) ; Wrap the Java HTTP response object in a Serf object.
		  (echo (:ticket-tag ticket)))
	      (! to body :no-metadata: :no-reply: echo)) ; Send the wrapped response back to the originator of the request.
	    (schedule-connection self authority (:ticket-uri ticket))
	    (audit
	     self "handle-response"
	     (format "~a context:~x ticket-id:~d" authority (java-object-to-id context) (:ticket-id ticket))))
	   (else
	    (audit
	     self "handle-response"
	     (format "~a context:~x no ticket" authority (java-object-to-id context)))))))) ; Oops. Something bad happened.

     (make-method!
      self connection/open ; HTTP connection listener. Connection is now open.
      (lambda (self connection) ; <java-nio-http-client-connection>
	(let* ((context   (get-context connection))
	       (authority (->string (get-attribute context :imposter.authority:))))
	  (hashtable/put! connections authority connection) ; Register the active connection.
	  (audit self "connection/open" (format "~a context:~x" authority (java-object-to-id context))))))

     (make-method!
      self connection/timeout ; HTTP connection listener. Connection timed out.
      (lambda (self connection) ; <java-nio-http-client-connection>
	(let* ((context   (get-context connection))
	       (authority (->string (get-attribute context :imposter.authority:))))
	  (hashtable/remove! connections authority) ; Rescind the connection.
	  (audit
	   self "connection/timeout"
	   (format "~a context:~x" authority (java-object-to-id context))))))

     (make-method!
      self connection/closed ; HTTP connection listener. Connection was closed.
      (lambda (self connection) ; <java-nio-http-client-connection>
	(let* ((context   (get-context connection))
	       (authority (->string (get-attribute context :imposter.authority:))))
	  (hashtable/remove! connections authority) ; Rescind the connection.
	  (audit
	   self "connection/closed"
	   (format "~a context:~x" authority (java-object-to-id context))))))

     (make-method!
      self connection/fatal-io-exception ; HTTP connection listener. Connection received fatal io error.
      (lambda (self connection) ; <java-nio-http-client-connection>
	(let* ((context   (get-context connection))
	       (authority (->string (get-attribute context :imposter.authority:))))
	  (hashtable/remove! connections authority) ; Rescind the connection.
	  (audit
	   self "connection/fatal-io-exception"
	   (format "~a context:~a" authority (java-object-to-id context))))))

     (make-method!
      self connection/fatal-protocol-exception ; HTTP connection listener. Connection received fatal protocol error.
      (lambda (self connection) ; <java-nio-http-client-connection>
	(let* ((context   (get-context connection))
	       (authority (->string (get-attribute context :imposter.authority:))))
	  (hashtable/remove! connections authority) ; Rescind the connection.
	  (audit
	   self "connection/fatal-protocol-exception"
	   (format "~a context:~a" authority (java-object-to-id context))))))

     (make-method!
      self session/cancelled ; HTTP session responder. Session was cancelled in mid-establishment.
      (lambda (self session) ; <java-session/request>
	(audit self "session/cancelled" (format "session:~x" (java-object-to-id session)))))

     (make-method!
      self session/completed ; HTTP session responder. Session establishment was successful.
      (lambda (self session) ; <java-session-request>
	(audit self "session/completed" (format "session:~x" (java-object-to-id session)))))

     (make-method!
      self session/failed ; HTTP session responder. Session establishment failed.
      (lambda (self session) ; <java-session-request>
	(audit self "session/failed" (format "session:~x" (java-object-to-id session)))))

     (make-method!
      self session/timeout ; HTTP session responder. Session establishment timed out.
      (lambda (self session) ; <java-session-request>
	(audit self "session/timeout" (format "session:~x" (java-object-to-id session)))))

     self))

;; Customizing the HTTP context.
(define :imposter.authority: (->jstring "imposter.authority"))
(define :imposter.pending:   (->jstring "imposter.pending"))
(define :http.connection:    (->jstring "http.connection"))
;(define (unwrap-authority context) (java-unwrap (get-attribute context :imposter.authority:)))
;(define (unwrap-pending context)   (java-unwrap (get-attribute context :imposter.pending:)))

;; Utility routines.

;; Given a <java.net.uri> u return a canonical address.
; (define (uri/canonical-address u)
;   (let ((address (java-new <java-internet-address> (get-host u) (get-port u))))
;    (canonical-address address)))

;; Returns #t iff uri is a legitimate URL for the HTTP protocol.
; (define (legal-http-uri? uri)
;   (and
;    (instance-of? uri <java.net.uri>)
;    (string-ci=? "http" (:scheme uri)) ; Is it http://... ?
;    (absolute? uri)))    ; The path must be absolute.

;; Returns #t if c is live, legitimate connection and #f otherwise.
; (define (connection-viable? c)
;   (and
;    (instance-of? c <java-default-nio-http-client-connection>)
;    (connection-alive? c)))

;; If no connection to authority exists then establish one.
;; If a connection exists and is viable then kick it.
;; If a connection exists and it is dead then try to replace it with a fresh connection.
;; If a ticket is enqueued for authority A and connection establishment
;; for A fails then the ticket will stay enqueued until another ticket arrives for A and stimulates another
;; round of connection establishment. This can be regarded as either a useful feature or a bug.

;; One solution is to have a nanny thread periodically tour the :outgoing table, authority by authority, attempting
;; to (re)establish a connection for any authority with enqueued tickets for which no viable connection exists
;; (not implemented here).

;; No current connection for the authority so (re)establish a connection.
;; Put a placeholder authority/() pair in :connections. The :connection-listener will replace
;; () with a fresh connection when it becomes available.

;; Utility routines for Imposter.

;; Construct an Apache HTTPComponents GET request
; (define (make-http-get-request uri)
;   (java-new
;    <java-basic-http-request>
;    (->jstring "GET")           ; Method for the request line (as Java String).
;    (uri-to-request-path uri))) ; Path (+ query if any) for the request line (as Java String).

